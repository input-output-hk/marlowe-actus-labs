module Actus.TestFramework where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractState, ContractTerms, Cycle, EventType, PRF, RiskFactors(..))
import Actus.Domain.ContractTerms (decodeDateTime)
import Actus.Utility ((<+>))
import Contrib.Data.Argonaut (decodeFromString)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, jsonParser)
import Data.Argonaut.Decode ((.:))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.DateTime (DateTime)
import Data.Decimal (Decimal, fromNumber, toSignificantDigits)
import Data.Decimal as Decimal
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Utils (trimStart) as String
import Data.Traversable (traverse)
import Effect.Exception (Error, error)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "Actus.TestFramework" do
    describe "execute" do
      it "actus-tests-pam.json" do
        jsonStr <- readTextFile UTF8 "./test/Actus/Domain/actus-tests-pam.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        (fixtures :: Object Json) <- either (throwError <<< error <<< show) pure do
          decodeJson json >>= traverse \pamJson -> do
            obj <- decodeJson pamJson
            pure $ obj

        forWithIndex_ fixtures \testId testCase -> do
          let
            (tc :: Either JsonDecodeError TestCase) = decodeJson testCase
          case tc of
            Left err -> fail (testId <> ": " <> show err)
            Right x -> runTest x

      pending "feature complete"

runTest :: forall a. MonadThrow Error a => TestCase -> a Unit
runTest tc =
  let
    cashFlows = genProjectedCashflows riskFactors tc.terms
  in
    assertTestResults cashFlows tc.results
  where
  riskFactors _ _ _ = defaultRiskFactors

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
      | otherwise = fail ("mismatch: " <> show tc.identifier <> " " <> show cf <> "\n" <> show a <> " vs. " <> show b)

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
  , values :: List ValueObserved
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

type TestCase =
  { identifier :: String
  , terms :: TestContractTerms
  ,
    -- to             :: Maybe DateTime,
    -- dataObserved   :: Map String DataObserved,
    -- eventsObserved :: List EventObserved,
    results :: List TestResult
  }

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
    let
      decodeDecimal = decodeFromString (String.trimStart >>> Decimal.fromString)
    obj <- decodeJson json

    evd <- obj .: "eventDate" >>= decodeDateTime
    evt <- obj .: "eventType"
    pay <- obj .: "payoff" >>= decodeDecimal
    cur <- obj .: "currency"

    pure $ TestResult
      { eventDate: evd
      , eventType: evt
      , payoff: pay
      , currency: cur
      }
--    TestResult <$> (obj .: "eventType") <*> (obj .: "payoff" >>= decodeDecimal) <*> (obj .: "currency")

