-- | Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Marlowe.Actus
  ( RiskFactorsMarlowe
  , genContract
  , defaultRiskFactors
  , toMarloweCashflow
  , evalVal
  , currencyToToken
  , currenciesWith6Decimals
  ) where

import Prelude

import Actus.Domain (CashFlow(..), ContractState, ContractTerms(..), EventType, Observation'(..), RiskFactors(..), Value'(..), _fromDecimal)
import Control.Apply (lift2)
import Data.Array (elem)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, fromDateTime)
import Data.Decimal (fromInt) as Decimal
import Data.Foldable (foldl)
import Data.List (List, reverse)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Language.Marlowe.Core.V1.Semantics.Types (Action(..), Bound(..), Case(..), ChoiceId(..), Contract(..), Observation(..), Party(..), Payee(..), Token(..), Value(..))

type ContractStateMarlowe = ContractState Value'
type RiskFactorsMarlowe = RiskFactors Value'

evalVal :: Value -> Maybe BigInt
evalVal (AvailableMoney _ _) = Nothing
evalVal (Constant integer) = Just integer
evalVal (NegValue val) = negate <$> evalVal val
evalVal (AddValue lhs rhs) = lift2 (+) (evalVal lhs) (evalVal rhs)
evalVal (SubValue lhs rhs) = lift2 (-) (evalVal lhs) (evalVal rhs)
evalVal (MulValue lhs rhs) = lift2 (*) (evalVal lhs) (evalVal rhs)
evalVal (DivValue lhs rhs) = do
  n <- evalVal lhs
  d <- evalVal rhs
  pure $
    if d == fromInt 0 then fromInt 0
    else n / d
evalVal (ChoiceValue _) = Nothing
evalVal (TimeIntervalStart) = Nothing
evalVal (TimeIntervalEnd) = Nothing
evalVal (UseValue _) = Nothing
evalVal (Cond cond thn els) = do
  obs <- evalObs cond
  if obs then evalVal thn else evalVal els

evalObs :: Observation -> Maybe Boolean
evalObs (AndObs lhs rhs) = lift2 (&&) (evalObs lhs) (evalObs rhs)
evalObs (OrObs lhs rhs) = lift2 (||) (evalObs lhs) (evalObs rhs)
evalObs (NotObs subObs) = not <$> evalObs subObs
evalObs (ChoseSomething _) = Nothing
evalObs (ValueGE lhs rhs) = lift2 (>=) (evalVal lhs) (evalVal rhs)
evalObs (ValueGT lhs rhs) = lift2 (>) (evalVal lhs) (evalVal rhs)
evalObs (ValueLT lhs rhs) = lift2 (<) (evalVal lhs) (evalVal rhs)
evalObs (ValueLE lhs rhs) = lift2 (<=) (evalVal lhs) (evalVal rhs)
evalObs (ValueEQ lhs rhs) = lift2 (==) (evalVal lhs) (evalVal rhs)
evalObs TrueObs = Just true
evalObs FalseObs = Just false

-- | Reduce the contract representation in size, the semantics of the
-- contract are not changed. TODO: formal verification
reduceContract :: Contract -> Contract
reduceContract Close = Close
reduceContract (Pay a b c d e) = Pay a b c (reduceValue d) (reduceContract e)
reduceContract (When cs t c) = When (map f cs) t (reduceContract c)
  where
  f (Case (Deposit a p o v) x) = Case (Deposit a p o (reduceValue v)) (reduceContract x)
  f (Case a x) = Case a (reduceContract x)
reduceContract c@(If obs a b) =
  case evalObs obs of
    Just o -> reduceContract (if o then a else b)
    Nothing -> c
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

toMarloweValue :: Value' -> Value
toMarloweValue (Constant' n) = Constant n
toMarloweValue (NegValue' n) = NegValue $ toMarloweValue n
toMarloweValue (AddValue' a b) = AddValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (SubValue' a b) = SubValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (MulValue' a b) = MulValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (DivValue' a b) = DivValue (toMarloweValue a) (toMarloweValue b)
toMarloweValue (Cond' o a b) = Cond (toMarloweObservation o) (toMarloweValue a) (toMarloweValue b)
toMarloweValue (ChoiceValue' i) = ChoiceValue i

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

toMarloweCashflow :: CashFlow Value' Party -> CashFlow Value Party
toMarloweCashflow
  ( CashFlow
      { contractId
      , party
      , counterparty
      , paymentDay
      , calculationDay
      , event
      , amount
      , notional
      , currency
      }
  ) = CashFlow
  { contractId
  , party
  , counterparty
  , paymentDay
  , calculationDay
  , event
  , amount: toMarloweValue amount
  , notional: toMarloweValue notional
  , currency
  }

-- | 'genContract' generates contract terms from projected cash-flows
genContract
  ::
     -- | List of projected cash-flows
     List (CashFlow Value' Party)
  ->
  -- | Marlowe contract
  Contract
genContract cashFlows = foldl generator Close $ reverse (map toMarloweCashflow cashFlows)
  where
  generator :: Contract -> CashFlow Value Party -> Contract
  generator continuation cashFlow@(CashFlow { paymentDay })
    | hasRiskFactor cashFlow =
        When
          [ Case
              (Choice (cashFlowToChoiceId cashFlow) [ Bound (fromInt 0) (fromInt 1000000000) ])
              (stub continuation cashFlow)
          ]
          (fromDateTime paymentDay)
          Close
  generator continuation cashFlow = stub continuation cashFlow

  stub continuation (CashFlow { amount, paymentDay, party, counterparty, currency }) =
    reduceContract $
      If
        ((Constant $ fromInt 0) `ValueLT` amount)
        ( invoice
            counterparty
            party
            (currencyToToken currency)
            (adjustDecimals currency amount)
            (fromDateTime paymentDay)
            continuation
        )
        ( If
            (amount `ValueLT` (Constant $ fromInt 0))
            ( invoice
                party
                counterparty
                (currencyToToken currency)
                (NegValue $ adjustDecimals currency amount)
                (fromDateTime paymentDay)
                continuation
            )
            continuation
        )

  invoice :: Party -> Party -> Token -> Value -> Instant -> Contract -> Contract
  invoice a b token amount timeout continue =
    When
      [ Case
          (Deposit a a token amount)
          ( Pay
              a
              (Party b)
              token
              amount
              continue
          )
      ]
      timeout
      Close

-- TODO: use token registry for handling of decimals
adjustDecimals :: String -> Value -> Value
adjustDecimals name v | elem name currenciesWith6Decimals = v
adjustDecimals _ v = DivValue v (Constant $ fromInt 1000000)

currenciesWith6Decimals :: Array String
currenciesWith6Decimals = [ "", "DjedTestUSD" ]

currencyToToken :: String -> Token
currencyToToken "DjedTestUSD" = Token "9772ff715b691c0444f333ba1db93b055c0864bec48fff92d1f2a7fe" "Djed_testMicroUSD"
currencyToToken i = Token "" i

cashFlowToChoiceId :: forall a b. CashFlow a b -> ChoiceId
cashFlowToChoiceId (CashFlow { event, paymentDay }) =
  let
    l = show event <> show paymentDay
  in
    ChoiceId l (Role "RiskFactor")

hasRiskFactor :: CashFlow Value Party -> Boolean
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

oracle :: Party
oracle = Address "" -- FIXME: oracle address

defaultRiskFactors :: ContractTerms -> EventType -> DateTime -> RiskFactorsMarlowe
defaultRiskFactors (ContractTerms { currency, settlementCurrency }) _ _ = -- FIXME: just a stub
  let
    o_rf_CURS = fromMaybe (_fromDecimal $ Decimal.fromInt 1) $ do
      cur <- currency
      settlementCur <- settlementCurrency
      if cur == settlementCur then Nothing
      else Just $ ChoiceValue' (ChoiceId (cur <> settlementCur) oracle)
  in
    RiskFactors
      { o_rf_CURS
      , o_rf_RRMO: ChoiceValue' (ChoiceId "rrmo" oracle) -- TODO: add to oracle
      , o_rf_SCMO: ChoiceValue' (ChoiceId "scmo" oracle) -- TODO: add to oracle
      , pp_payoff: ChoiceValue' (ChoiceId "pp" oracle) -- TODO: add to oracle
      }
