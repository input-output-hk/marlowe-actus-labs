module Actus.TestFramework where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractState, ContractTerms(..), Cycle, EventType(..), PRF, RiskFactors(..), setDefaultContractTermValues)
import Actus.Domain.ContractTerms (decodeDateTime, decodeDecimal)
import Actus.Utility (applyBDCWithCfg, (<+>))
import Contrib.Data.Argonaut.Generic.Record (decodeRecord)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, (.:?))
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.DateTime (DateTime)
import Data.Decimal (Decimal, fromNumber, toSignificantDigits)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..), find, (:))
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (Error)
import Foreign.Object (Object)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

spec :: Object (String /\ Either JsonDecodeError TestCase) -> Spec Unit
spec fixture = do
  describe "Actus.TestFramework" do
    describe "execute" do
      for_ fixture \(testId /\ tc) -> do
        it ("testId: " <> testId) do
          case tc of
            Left err -> fail (testId <> ": " <> show err)
            Right x -> runTest x
      pending "feature complete"

runTest :: forall a. MonadThrow Error a => TestCase -> a Unit
runTest tc =
  let
    cashFlows = genProjectedCashflows riskFactors (setDefaultContractTermValues tc.terms)
  in
    assertTestResults cashFlows tc.results
  where
  assertTestResults :: forall b. MonadThrow Error b => List TestCashFlow -> List TestResult -> b Unit
  assertTestResults Nil Nil = pure unit
  assertTestResults (cf : cfs) (r : rs) = assertTestResult cf r *> assertTestResults cfs rs
  assertTestResults _ _ = fail "Sizes differ"

  assertTestResult :: forall m. MonadThrow Error m => CashFlow Decimal -> TestResult -> m Unit
  assertTestResult (cf@(CashFlow { cashEvent, amount, cashPaymentDay })) (TestResult { eventType, payoff, eventDate }) = do
    assertEqual cashEvent eventType
    assertEqual cashPaymentDay eventDate
    assertEqual (toSignificantDigits 8 amount) (toSignificantDigits 8 payoff)
    where
    assertEqual :: forall b n. Show b => Eq b => MonadThrow Error n => b -> b -> n Unit
    assertEqual a b
      | a == b = pure unit
      | otherwise = fail ("mismatch: " <> show tc.identifier <> " " <> show cf <> "\n" <> show a <> " vs. " <> show b <> "\n" <> show tc.terms)

  riskFactors i ev date =
    case getValue i tc ev date of
      Just v -> case ev of
        RR -> let RiskFactors rf = defaultRiskFactors in RiskFactors $ rf { o_rf_RRMO = v }
        SC -> let RiskFactors rf = defaultRiskFactors in RiskFactors $ rf { o_rf_SCMO = v }
        DV -> let RiskFactors rf = defaultRiskFactors in RiskFactors $ rf { dv_payoff = v }
        XD -> let RiskFactors rf = defaultRiskFactors in RiskFactors $ rf { xd_payoff = v }
        _ -> let RiskFactors rf = defaultRiskFactors in RiskFactors $ rf { o_rf_CURS = v }
      Nothing -> defaultRiskFactors

getValue :: String -> TestCase -> EventType -> DateTime -> Maybe Decimal
getValue _ { terms, dataObserved } ev date =
  do
    let (ContractTerms ct) = terms
    key <- observedKey terms ev
    DataObserved { values } <- lookup key dataObserved
    ValueObserved { value } <-
      find
        ( \(ValueObserved { timestamp }) ->
            let
              { calculationDay } = applyBDCWithCfg ct.scheduleConfig timestamp
            in
              calculationDay == date
        )
        values
    pure value
  where
  observedKey (ContractTerms ct) RR = ct.marketObjectCodeOfRateReset
  observedKey (ContractTerms ct) SC = ct.marketObjectCodeOfScalingIndex
  observedKey (ContractTerms ct) _ = ct.settlementCurrency

-- |Unscheduled events from test cases
applySettlementPeriod :: Maybe Cycle -> DateTime -> DateTime
applySettlementPeriod (Just c) s = s <+> c
applySettlementPeriod Nothing s = s

defaultRiskFactors :: RiskFactors Decimal
defaultRiskFactors = RiskFactors
  { o_rf_CURS: fromNumber 1.0
  , o_rf_RRMO: fromNumber 1.0
  , o_rf_SCMO: fromNumber 1.0
  , pp_payoff: fromNumber 0.0
  , xd_payoff: fromNumber 0.0
  , dv_payoff: fromNumber 0.0
  }

type TestCashFlow = CashFlow Decimal
type TestContractState = ContractState Decimal
type TestContractTerms = ContractTerms Decimal

data DataObserved = DataObserved
  { identifier :: String
  , values :: Array ValueObserved
  }

data ValueObserved = ValueObserved
  { timestamp :: DateTime
  , value :: Decimal
  }

data EventObserved = EventObserved
  { time :: DateTime
  , eventType :: EventType
  , value :: Decimal
  , contractId :: Maybe String
  , states :: Maybe PRF
  }

instance DecodeJson ValueObserved where
  decodeJson json = do
    obj <- decodeJson json
    ts <- obj .: "timestamp" >>= decodeDateTime
    val <- obj .: "value" >>= decodeDecimal
    pure $ ValueObserved
      { timestamp: ts
      , value: val
      }

instance DecodeJson DataObserved where
  decodeJson json = do
    obj <- decodeJson json
    identifier <- obj .: "identifier"
    values <- obj .: "data"
    pure $ DataObserved
      { identifier
      , values
      }

instance DecodeJson EventObserved where
  decodeJson json = do
    obj <- decodeJson json
    ts <- obj .: "time" >>= decodeDateTime
    evt <- obj .: "eventType"
    val <- obj .: "value" >>= decodeDecimal
    cid <- obj .:? "contractId"
    sts <- obj .:? "states"
    pure $ EventObserved
      { time: ts
      , eventType: evt
      , value: val
      , contractId: cid
      , states: sts
      }

type TestCase =
  { identifier :: String
  , terms :: TestContractTerms
  -- , to :: Maybe DateTime
  , dataObserved :: Map String DataObserved
  , eventsObserved :: List EventObserved
  , results :: List TestResult
  }

decodeTestCase :: Json -> Either JsonDecodeError TestCase
decodeTestCase = decodeRecord decoders
  where
  decoders = { dataObserved: map dataObservedInternal :: Maybe _ -> Maybe _ }

  dataObservedInternal :: Json -> Either JsonDecodeError (Map String DataObserved)
  dataObservedInternal json = do
    (obj :: Object _) <- decodeJson json
    pure $ Map.fromFoldableWithIndex obj

newtype TestResult = TestResult
  { eventDate :: DateTime
  , eventType :: EventType
  , payoff :: Decimal
  , currency :: String
  -- notionalPrincipal   :: Decimal
  -- exerciseAmount      :: Maybe Decimal,
  -- nominalInterestRate :: Maybe Decimal,
  -- accruedInterest     :: Maybe Decimal
  }

instance DecodeJson TestResult where
  decodeJson json = do
    obj <- decodeJson json
    TestResult <$> ado
      eventDate <- obj .: "eventDate" >>= decodeDateTime
      eventType <- obj .: "eventType"
      payoff <- obj .: "payoff" >>= decodeDecimal
      currency <- obj .: "currency"
      in { eventDate, eventType, payoff, currency }
