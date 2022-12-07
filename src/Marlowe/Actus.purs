-- | Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Marlowe.Actus
  ( CashFlowMarlowe
  , ContractTermsMarlowe
  , RiskFactorsMarlowe
  , genContract
  -- == Conversion from Decimal to Marlowe representation
  -- re-export
  -- utility
  , toMarlowe
  ) where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractState, ContractTerms(..), EventType, Observation'(..), RiskFactors, Value'(..), marloweFixedPoint)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Foldable (foldl)
import Data.List (reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Semantics as MarloweSemantics
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Bound(..), Case(..), ChoiceId(..), Contract(..), Observation(..), Party(..), Payee(..), TimeInterval(..), Value(..), adaToken)
import Language.Marlowe.Core.V1.Semantics.Types as MarloweCore
import Marlowe.Time (unixEpoch)

type CashFlowMarlowe = CashFlow Value
type ContractStateMarlowe = ContractState Value'
type ContractTermsMarlowe = ContractTerms Value'
type RiskFactorsMarlowe = RiskFactors Value'

evalVal :: Value -> BigInt
evalVal d = MarloweSemantics.evalValue env state d
  where
  env = MarloweCore.Environment { timeInterval: TimeInterval unixEpoch unixEpoch }
  state = MarloweSemantics.emptyState

evalObs :: Observation -> Boolean
evalObs d = MarloweSemantics.evalObservation env state d
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
reduceContract (If obs a b) =
  let
    c = evalObs obs
  in
    reduceContract (if c then a else b)
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
reduceValue v = Constant $ evalVal v

toMarloweValue :: Value' -> Value
toMarloweValue (Constant' n) = Constant n
toMarloweValue (NegValue' n) = NegValue $ toMarloweValue n
toMarloweValue (AddValue' a b) = AddValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (SubValue' a b) = SubValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (MulValue' a b) = MulValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (Cond' o a b) = Cond (toMarloweObservation o) (toMarloweValue a) (toMarloweValue b)

toMarloweObservation :: Observation' -> Observation
toMarloweObservation (AndObs' a b) = AndObs (toMarloweObservation a) (toMarloweObservation b)
toMarloweObservation (OrObs' a b) = OrObs (toMarloweObservation a) (toMarloweObservation b)
toMarloweObservation (NotObs' a) = NotObs $ toMarloweObservation a
toMarloweObservation (ValueGE' a b) = ValueGE (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueGT' a b) = ValueGT (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueLT' a b) = ValueLT (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueLE' a b) = ValueLE (toMarloweValue a) (toMarloweValue b)
toMarloweObservation (ValueEQ' a b) = ValueEQ (toMarloweValue a) (toMarloweValue b)
toMarloweObservation TrueObs' = TrueObs
toMarloweObservation FalseObs' = FalseObs

toMarloweCashflow :: CashFlow Value' -> CashFlow Value
toMarloweCashflow
  ( CashFlow
      { tick
      , cashParty
      , cashCounterParty
      , cashPaymentDay
      , cashCalculationDay
      , cashEvent
      , amount
      , notional
      , cashCurrency
      }
  ) = CashFlow
  { tick: tick
  , cashParty: cashParty
  , cashCounterParty: cashCounterParty
  , cashPaymentDay: cashPaymentDay
  , cashCalculationDay: cashCalculationDay
  , cashEvent: cashEvent
  , amount: toMarloweValue amount
  , notional: toMarloweValue notional
  , cashCurrency: cashCurrency
  }

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
    cfs = genProjectedCashflows rf ct
  in
    foldl gen Close $ reverse (map toMarloweCashflow cfs)
  where
  gen :: Contract -> CashFlow Value -> Contract
  gen cont cf@(CashFlow { cashPaymentDay })
    | hasRiskFactor cf =
        When
          [ Case
              (Choice (cashFlowToChoiceId cf) [ Bound (fromInt 0) (fromInt 1000000000) ])
              (stub cont cf)
          ]
          (fromDateTime cashPaymentDay)
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
      timeout
      Close

cashFlowToChoiceId :: forall a. CashFlow a -> ChoiceId
cashFlowToChoiceId (CashFlow { cashEvent, cashPaymentDay }) =
  let
    l = show cashEvent <> show cashPaymentDay
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
  hasRiskFactor' (Cond _ a b) = hasRiskFactor' a || hasRiskFactor' b

toMarlowe :: ContractTerms Decimal -> ContractTermsMarlowe
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
    , marketObjectCode: Nothing
    , contractPerformance: ct.contractPerformance
    , creditEventTypeCovered: ct.creditEventTypeCovered
    , coverageOfCreditEnhancement: constant =<< ct.coverageOfCreditEnhancement
    , guaranteedExposure: ct.guaranteedExposure
    , cycleOfFee: ct.cycleOfFee
    , cycleAnchorDateOfFee: ct.cycleAnchorDateOfFee
    , feeAccrued: constant =<< ct.feeAccrued
    , feeBasis: ct.feeBasis
    , feeRate: constant =<< ct.feeRate
    , cycleAnchorDateOfInterestPayment: ct.cycleAnchorDateOfInterestPayment
    , cycleOfInterestPayment: ct.cycleOfInterestPayment
    , accruedInterest: constant =<< ct.accruedInterest
    , capitalizationEndDate: ct.capitalizationEndDate
    , cycleAnchorDateOfInterestCalculationBase: ct.cycleAnchorDateOfInterestCalculationBase
    , cycleOfInterestCalculationBase: ct.cycleOfInterestCalculationBase
    , interestCalculationBase: ct.interestCalculationBase
    , interestCalculationBaseAmount: constant =<< ct.interestCalculationBaseAmount
    , nominalInterestRate: constant =<< ct.nominalInterestRate
    , nominalInterestRate2: constant =<< ct.nominalInterestRate2
    , interestScalingMultiplier: constant =<< ct.interestScalingMultiplier
    , notionalPrincipal: constant =<< ct.notionalPrincipal
    , premiumDiscountAtIED: constant =<< ct.premiumDiscountAtIED
    , maturityDate: ct.maturityDate
    , amortizationDate: ct.amortizationDate
    , exerciseDate: ct.exerciseDate
    , cycleAnchorDateOfPrincipalRedemption: ct.cycleAnchorDateOfPrincipalRedemption
    , cycleOfPrincipalRedemption: ct.cycleOfPrincipalRedemption
    , nextPrincipalRedemptionPayment: constant =<< ct.nextPrincipalRedemptionPayment
    , purchaseDate: ct.purchaseDate
    , priceAtPurchaseDate: constant =<< ct.priceAtPurchaseDate
    , terminationDate: ct.terminationDate
    , priceAtTerminationDate: constant =<< ct.priceAtTerminationDate
    , quantity: constant =<< ct.quantity
    , currency: ct.currency
    , currency2: ct.currency2
    , scalingIndexAtStatusDate: constant =<< ct.scalingIndexAtStatusDate
    , cycleAnchorDateOfScalingIndex: ct.cycleAnchorDateOfScalingIndex
    , cycleOfScalingIndex: ct.cycleOfScalingIndex
    , scalingEffect: ct.scalingEffect
    , scalingIndexAtContractDealDate: constant =<< ct.scalingIndexAtContractDealDate
    , marketObjectCodeOfScalingIndex: ct.marketObjectCodeOfScalingIndex
    , notionalScalingMultiplier: constant =<< ct.notionalScalingMultiplier
    , cycleOfOptionality: ct.cycleOfOptionality
    , cycleAnchorDateOfOptionality: ct.cycleAnchorDateOfOptionality
    , optionType: ct.optionType
    , optionStrike1: constant =<< ct.optionStrike1
    , optionExerciseType: ct.optionExerciseType
    , settlementPeriod: ct.settlementPeriod
    , deliverySettlement: ct.deliverySettlement
    , exerciseAmount: constant =<< ct.exerciseAmount
    , futuresPrice: constant =<< ct.futuresPrice
    , penaltyRate: constant =<< ct.penaltyRate
    , penaltyType: ct.penaltyType
    , prepaymentEffect: ct.prepaymentEffect
    , cycleOfRateReset: ct.cycleOfRateReset
    , cycleAnchorDateOfRateReset: ct.cycleAnchorDateOfRateReset
    , nextResetRate: constant =<< ct.nextResetRate
    , rateSpread: constant =<< ct.rateSpread
    , rateMultiplier: constant =<< ct.rateMultiplier
    , periodFloor: constant =<< ct.periodFloor
    , periodCap: constant =<< ct.periodCap
    , lifeCap: constant =<< ct.lifeCap
    , lifeFloor: constant =<< ct.lifeFloor
    , marketObjectCodeOfRateReset: ct.marketObjectCodeOfRateReset
    , cycleOfDividendPayment: ct.cycleOfDividendPayment
    , cycleAnchorDateOfDividendPayment: ct.cycleAnchorDateOfDividendPayment
    , nextDividendPaymentAmount: constant =<< ct.nextDividendPaymentAmount
    , enableSettlement: ct.enableSettlement
    }
  where
  constant :: Decimal -> Maybe Value'
  constant n = Constant' <$> toMarloweFixedPoint n

  toMarloweFixedPoint :: Decimal -> Maybe BigInt
  toMarloweFixedPoint = BigInt.fromString <<< Decimal.toString <<< Decimal.floor <<< ((Decimal.fromInt marloweFixedPoint) * _)
