module Test.Marlowe.Actus where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError, decodeJson, jsonParser)
import Data.Array (toUnfoldable)
import Data.BigInt.Argonaut (BigInt, fromInt, fromString, quot)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.List (List, filter)
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Language.Marlowe.Core.V1.Semantics (isClose, playTrace)
import Language.Marlowe.Core.V1.Semantics.Types (Input(..), Party(..), Payee(..), Payment(..), TimeInterval(..), Token(..), TransactionInput(..), TransactionOutput(..))
import Marlowe.Actus (genContract)
import Marlowe.Time (unixEpoch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec as Spec
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "Marlowe.Actus" $ Spec.parallel do
    it "Contract generation" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/ex_pam1.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError ContractTerms) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            party = Role "party"
            counterparty = Role "counterparty"
            cashFlows = genProjectedCashflows (party /\ counterparty) riskFactors $ contract
            marloweContract = genContract cashFlows

            payin = IDeposit party party (Token "" "") $ unsafePartial $ fromJust $ fromString "10000000000"
            interest = IDeposit counterparty counterparty (Token "" "") $ unsafePartial $ fromJust $ fromString "200000000"
            payout = IDeposit counterparty counterparty (Token "" "") $ unsafePartial $ fromJust $ fromString "10000000000"

            inputs = toUnfoldable
              [ payin
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , interest
              , payout
              ]

            interval = TimeInterval unixEpoch unixEpoch
            output = playTrace unixEpoch marloweContract (toUnfoldable [ TransactionInput { interval, inputs } ])

          case output of
            Error _ -> fail "PlayTrace error"
            TransactionOutput out -> do
              shouldEqual (isClose out.txOutContract) true
              shouldEqual (totalPayments (Party party) out.txOutPayments) (fromInt 12000)
              shouldEqual (totalPayments (Party counterparty) out.txOutPayments) (fromInt 10000)

totalPayments :: Payee -> List Payment -> BigInt
totalPayments payee = fromMarloweFixedPoint <<< sum <<< map m <<< filter f
  where
  m (Payment _ _ (Token "" "") amt) = amt
  m _ = fromInt 0
  f (Payment _ pay _ _) = pay == payee
  fromMarloweFixedPoint i = i `quot` (fromInt marloweFixedPoint)

riskFactors :: EventType -> DateTime -> RiskFactors Value'
riskFactors _ _ = RiskFactors
  { o_rf_CURS: fromInt 1
  , o_rf_RRMO: fromInt 1
  , o_rf_SCMO: fromInt 1
  , pp_payoff: fromInt 0
  }
  where
  fromInt = Constant' <<< BigInt.fromInt <<< (marloweFixedPoint * _)
