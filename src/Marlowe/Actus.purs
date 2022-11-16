-- | Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Marlowe.Actus
  ( CashFlowMarlowe
  , ContractTermsMarlowe
  , RiskFactorsMarlowe
  --, defaultRiskFactors
  , genContract
  -- == Conversion from Number to Marlowe representation
  -- re-export
  , module Actus.Domain
  -- utility
  , fromMarloweFixedPoint
  , toMarlowe
  , toMarloweFixedPoint
  ) where

import Actus.Domain
import Language.Marlowe.Extended.V1
import Prelude

import Actus.Core (genProjectedCashflows)
import Control.Alt (map)
import Data.BigInt.Argonaut (BigInt, abs, fromInt, quot, rem)
import Data.DateTime (DateTime(..))
import Data.DateTime.Instant (Instant, fromDateTime, instant)
import Data.Foldable (foldl)
import Data.Function ((#), ($))
import Data.Int (round)
import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Ord (signum)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Semantics as MarloweSemantics
import Language.Marlowe.Core.V1.Semantics.Types (Bound(..), ChoiceId(..), Party(..), TimeInterval(..), adaToken)
import Language.Marlowe.Core.V1.Semantics.Types as MarloweCore
import Marlowe.Time (unixEpoch)

type CashFlowMarlowe = CashFlow Value
type ContractStateMarlowe = ContractState Value
type ContractTermsMarlowe = ContractTerms Value
type RiskFactorsMarlowe = RiskFactors Value

marloweFixedPoint :: BigInt
marloweFixedPoint = fromInt 1000000

toMarloweFixedPoint :: Number -> BigInt
toMarloweFixedPoint n = marloweFixedPoint * fromInt (round n)

fromMarloweFixedPoint :: BigInt -> BigInt
fromMarloweFixedPoint i = i `quot` marloweFixedPoint

-- In order to have manageble contract sizes, we need to reduce Value as
-- good as possible.
--
-- Note:
--
-- * This interfers with the semantics - ideally we would have formally
--   verified reduction semantics instead
--
-- * There are partial implementations, as the evaluation of a Value
--   is not always possible
--
-- * The semantics of division, i.e. rounding changed. In order to
--   preserve financial rounding, in ACTUS we always have to use `division`
--   rather than `DivValue`

-- instance Num Value where
--   x + y = reduceValue $ AddValue x y
--   x - y = reduceValue $ SubValue x y
--   x * y = reduceValue $ division (MulValue x y) (Constant marloweFixedPoint)
--   abs a = _max a (NegValue a)
--     where
--       _max x y = Cond (ValueGT x y) x y
--   fromInteger n = Constant $ n * marloweFixedPoint
--   negate a = NegValue a
--   signum a =
--     Cond (ValueLT a 0) (-1) $
--       Cond (ValueGT a 0) 1 0

--instance ActusFrac Value where
--  _ceiling x = fromMaybe (error "ActusFrac partially implemented") (evalVal x)

-- instance Fractional Value where
--   lhs / rhs = MulValue (division lhs rhs) (Constant marloweFixedPoint)
--   fromRational (x :% y) = MulValue (division (fromInteger x) (fromInteger y)) (Constant marloweFixedPoint)

-- |Division with financial rounding
division :: Value -> Value -> Value
division lhs rhs = fromMaybe (Constant $ fromInt 0) $ -- TODO: remove fromMaybe

  do
    n <- evalVal lhs
    d <- evalVal rhs
    pure $ Constant (division' n d)
  where
  -- division' :: BigInt -> BigInt -> BigInt
  division' x _ | x == fromInt 0 = fromInt 0
  division' _ y | y == fromInt 0 = fromInt 0
  division' n d =
    let
      q = n `quot` d
      r = n `rem` d
      ar = abs r * (fromInt 2)
      ad = abs d
    in
      if ar < ad then q -- reminder < 1/2
      else if ar > ad then q + signum n * signum d -- reminder > 1/2
      else
        let -- reminder == 1/2
          qIsEven = q `rem` (fromInt 2) == (fromInt 0)
        in
          if qIsEven then q else q + signum n * signum d

evalVal :: Value -> Maybe BigInt
evalVal d = MarloweSemantics.evalValue env state <$> toCore d
  where
  env = MarloweCore.Environment { timeInterval: TimeInterval unixEpoch unixEpoch }
  state = MarloweSemantics.emptyState

evalObs :: Observation -> Maybe Boolean
evalObs d = MarloweSemantics.evalObservation env state <$> toCore d
  where
  env = MarloweCore.Environment { timeInterval: TimeInterval unixEpoch unixEpoch }
  state = MarloweSemantics.emptyState

-- | Reduce the contract representation in size, the semantics of the
-- contract are not changed. TODO: formal verification
reduceContract :: Contract -> Contract
reduceContract Close = Close
reduceContract (Pay a b c d e) = Pay a b c (reduceValue d) (reduceContract e)
reduceContract (When cs t c) = When (map f cs) t (reduceContract c)
  where
  f (Case a x) = Case a (reduceContract x)
reduceContract i@(If obs a b) = case evalObs obs of
  Just c -> reduceContract (if c then a else b)
  Nothing -> i
reduceContract (Let v o c) = Let v (reduceValue o) (reduceContract c)
reduceContract (Assert o c) = Assert (reduceObs o) (reduceContract c)

reduceObs :: Observation -> Observation
reduceObs (AndObs a b) = AndObs (reduceObs a) (reduceObs b)
reduceObs (OrObs a b) = OrObs (reduceObs a) (reduceObs b)
reduceObs (NotObs a) = NotObs (reduceObs a)
reduceObs (ValueGE a b) = ValueGE (reduceValue a) (reduceValue b)
reduceObs (ValueGT a b) = ValueGT (reduceValue a) (reduceValue b)
reduceObs (ValueLE a b) = ValueLE (reduceValue a) (reduceValue b)
reduceObs (ValueLT a b) = ValueLT (reduceValue a) (reduceValue b)
reduceObs (ValueEQ a b) = ValueEQ (reduceValue a) (reduceValue b)
reduceObs x = x

reduceValue :: Value -> Value
reduceValue v = maybe v Constant (evalVal v)

-- | 'genContract' validatates the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genContract
  ::
     -- | Party and Counter-party for the contract
     Party /\ Party
  ->
  -- | Risk factors per event and time
  (String -> EventType -> DateTime -> RiskFactorsMarlowe)
  ->
  -- | ACTUS contract terms
  ContractTermsMarlowe
  ->
  -- | Marlowe contract
  Contract
genContract (party /\ couterparty) rf ct =
  let
    cfs = Nil -- FIXME: genProjectedCashflows rf ct
  in
    foldl gen Close $ reverse cfs
  where
  gen :: Contract -> CashFlow Value -> Contract
  gen cont cf@(CashFlow { cashPaymentDay })
    | hasRiskFactor cf =
        When
          [ Case
              (Choice (cashFlowToChoiceId cf) [ Bound (fromInt 0) (fromInt 1000000000) ])
              (stub cont cf)
          ]
          (TimeValue $ fromDateTime cashPaymentDay)
          Close
  gen cont cf = stub cont cf

  stub cont (CashFlow { amount, cashPaymentDay }) =
    let
      t = fromDateTime cashPaymentDay
      c = reduceContract cont
    in
      reduceContract $
        If
          ((Constant $ fromInt 0) `ValueLT` amount)
          ( invoice
              couterparty
              party
              amount
              t
              c
          )
          ( If
              (amount `ValueLT` (Constant $ fromInt 0))
              ( invoice
                  party
                  couterparty
                  (NegValue amount)
                  t
                  c
              )
              c
          )

  invoice :: Party -> Party -> Value -> Instant -> Contract -> Contract
  invoice a b amount timeout continue =
    When
      [ Case
          (Deposit a a adaToken amount)
          ( Pay
              a
              (Party b)
              adaToken
              amount
              continue
          )
      ]
      (TimeValue timeout)
      Close

cashFlowToChoiceId :: forall a. CashFlow a -> ChoiceId
cashFlowToChoiceId (CashFlow { cashEvent, cashPaymentDay }) =
  let
    l = "" -- FIXME: show cashEvent <> show cashPaymentDay
  in
    ChoiceId l (Role "RiskFactor")

hasRiskFactor :: CashFlow Value -> Boolean
hasRiskFactor cf@(CashFlow { amount }) = hasRiskFactor' amount
  where
  hasRiskFactor' :: Value -> Boolean
  hasRiskFactor' (ChoiceValue j) | cashFlowToChoiceId cf == j = true
  hasRiskFactor' (ChoiceValue _) = false
  hasRiskFactor' (Constant _) = false
  hasRiskFactor' (AvailableMoney _ _) = false
  hasRiskFactor' (UseValue _) = false
  hasRiskFactor' (AddValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (SubValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (MulValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (DivValue a b) = hasRiskFactor' a || hasRiskFactor' b
  hasRiskFactor' (NegValue a) = hasRiskFactor' a
  hasRiskFactor' TimeIntervalStart = false
  hasRiskFactor' TimeIntervalEnd = false
  hasRiskFactor' (ConstantParam _) = false
  hasRiskFactor' (Cond _ a b) = hasRiskFactor' a || hasRiskFactor' b

--defaultRiskFactors :: String -> EventType -> DateTime -> RiskFactors Value
--defaultRiskFactors _ ev t =
--  let choiceId = ChoiceId (show ev <> show t) (Role "RiskFactor")
--      value = ChoiceValue choiceId
--  in mkRiskFactor ev value

-- mkRiskFactor :: EventType -> Value -> RiskFactors Value
-- mkRiskFactor PP value =
--   RiskFactors
--     { o_rf_CURS : one,
--       o_rf_RRMO : one,
--       o_rf_SCMO : one,
--       pp_payoff : value,
--       xd_payoff : zero,
--       dv_payoff : zero
--     }
-- mkRiskFactor XD value =
--   RiskFactors
--     { o_rf_CURS : one,
--       o_rf_RRMO : one,
--       o_rf_SCMO : one,
--       pp_payoff : zero,
--       xd_payoff : value,
--       dv_payoff : zero
--     }
-- mkRiskFactor DV value =
--   RiskFactors
--     { o_rf_CURS : one,
--       o_rf_RRMO : one,
--       o_rf_SCMO : one,
--       pp_payoff : zero,
--       xd_payoff : zero,
--       dv_payoff : value
--     }
-- mkRiskFactor _ _ =
--   RiskFactors
--     { o_rf_CURS : one,
--       o_rf_RRMO : one,
--       o_rf_SCMO : one,
--       pp_payoff : zero,
--       xd_payoff : zero,
--       dv_payoff : zero
--     }

constant :: Number -> Value
constant n = Constant $ toMarloweFixedPoint n

toMarlowe :: ContractTerms Number -> ContractTermsMarlowe
toMarlowe (ContractTerms ct) =
  ContractTerms
    { contractId: ct.contractId
    , contractType: ct.contractType
    , contractRole: ct.contractRole
    , settlementCurrency: ct.settlementCurrency
    , initialExchangeDate: ct.initialExchangeDate
    , dayCountConvention: ct.dayCountConvention
    , scheduleConfig: ct.scheduleConfig
    , statusDate: ct.statusDate
    , marketObjectCodeRef: Nothing
    , contractPerformance: ct.contractPerformance
    , creditEventTypeCovered: ct.creditEventTypeCovered
    , coverageOfCreditEnhancement: constant <$> ct.coverageOfCreditEnhancement
    , guaranteedExposure: ct.guaranteedExposure
    , cycleOfFee: ct.cycleOfFee
    , cycleAnchorDateOfFee: ct.cycleAnchorDateOfFee
    , feeAccrued: constant <$> ct.feeAccrued
    , feeBasis: ct.feeBasis
    , feeRate: constant <$> ct.feeRate
    , cycleAnchorDateOfInterestPayment: ct.cycleAnchorDateOfInterestPayment
    , cycleOfInterestPayment: ct.cycleOfInterestPayment
    , accruedInterest: constant <$> ct.accruedInterest
    , capitalizationEndDate: ct.capitalizationEndDate
    , cycleAnchorDateOfInterestCalculationBase: ct.cycleAnchorDateOfInterestCalculationBase
    , cycleOfInterestCalculationBase: ct.cycleOfInterestCalculationBase
    , interestCalculationBase: ct.interestCalculationBase
    , interestCalculationBaseA: constant <$> ct.interestCalculationBaseA
    , nominalInterestRate: constant <$> ct.nominalInterestRate
    , nominalInterestRate2: constant <$> ct.nominalInterestRate2
    , interestScalingMultiplier: constant <$> ct.interestScalingMultiplier
    , notionalPrincipal: constant <$> ct.notionalPrincipal
    , premiumDiscountAtIED: constant <$> ct.premiumDiscountAtIED
    , maturityDate: ct.maturityDate
    , amortizationDate: ct.amortizationDate
    , exerciseDate: ct.exerciseDate
    , cycleAnchorDateOfPrincipalRedemption: ct.cycleAnchorDateOfPrincipalRedemption
    , cycleOfPrincipalRedemption: ct.cycleOfPrincipalRedemption
    , nextPrincipalRedemptionPayment: constant <$> ct.nextPrincipalRedemptionPayment
    , purchaseDate: ct.purchaseDate
    , priceAtPurchaseDate: constant <$> ct.priceAtPurchaseDate
    , terminationDate: ct.terminationDate
    , priceAtTerminationDate: constant <$> ct.priceAtTerminationDate
    , quantity: constant <$> ct.quantity
    , currency: ct.currency
    , currency2: ct.currency2
    , scalingIndexAtStatusDate: constant <$> ct.scalingIndexAtStatusDate
    , cycleAnchorDateOfScalingIndex: ct.cycleAnchorDateOfScalingIndex
    , cycleOfScalingIndex: ct.cycleOfScalingIndex
    , scalingEffect: ct.scalingEffect
    , scalingIndexAtContractDealDate: constant <$> ct.scalingIndexAtContractDealDate
    , marketObjectCodeOfScalingIndex: ct.marketObjectCodeOfScalingIndex
    , notionalScalingMultiplier: constant <$> ct.notionalScalingMultiplier
    , cycleOfOptionality: ct.cycleOfOptionality
    , cycleAnchorDateOfOptionality: ct.cycleAnchorDateOfOptionality
    , optionType: ct.optionType
    , optionStrike1: constant <$> ct.optionStrike1
    , optionExerciseType: ct.optionExerciseType
    , settlementPeriod: ct.settlementPeriod
    , deliverySettlement: ct.deliverySettlement
    , exerciseAmount: constant <$> ct.exerciseAmount
    , futuresPrice: constant <$> ct.futuresPrice
    , penaltyRate: constant <$> ct.penaltyRate
    , penaltyType: ct.penaltyType
    , prepaymentEffect: ct.prepaymentEffect
    , cycleOfRateReset: ct.cycleOfRateReset
    , cycleAnchorDateOfRateReset: ct.cycleAnchorDateOfRateReset
    , nextResetRate: constant <$> ct.nextResetRate
    , rateSpread: constant <$> ct.rateSpread
    , rateMultiplier: constant <$> ct.rateMultiplier
    , periodFloor: constant <$> ct.periodFloor
    , periodCap: constant <$> ct.periodCap
    , lifeCap: constant <$> ct.lifeCap
    , lifeFloor: constant <$> ct.lifeFloor
    , marketObjectCodeOfRateReset: ct.marketObjectCodeOfRateReset
    , cycleOfDividend: ct.cycleOfDividend
    , cycleAnchorDateOfDividend: ct.cycleAnchorDateOfDividend
    , nextDividendPaymentAmount: constant <$> ct.nextDividendPaymentAmount
    , enableSettlement: ct.enableSettlement
    }
