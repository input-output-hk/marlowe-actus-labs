module Test.Marlowe.Actus where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError, decodeJson, jsonParser)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Either (Either(..), either)
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Language.Marlowe.Core.V1.Semantics (isClose)
import Language.Marlowe.Core.V1.Semantics.Types (Party(..))
import Marlowe.Actus (genContract, toMarlowe)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "Marlowe.Actus" $ Spec.parallel do
    it "Contract generation" do
      jsonStr <- readTextFile UTF8 "./test/Marlowe/Actus/pam01.json"
      json <- either (throwError <<< error) pure $ jsonParser jsonStr

      let
        (terms :: Either JsonDecodeError (ContractTerms Decimal)) = decodeJson json
      case terms of
        Left err -> fail (show err)
        Right contract -> do
          let
            cashFlows = genProjectedCashflows (Address "addr1" /\ Address "addr2") riskFactors $ toMarlowe contract
            marloweContract = genContract cashFlows
          if isClose marloweContract then fail "Contract is not supposed to be Close" else pure unit

riskFactors :: EventType -> DateTime -> RiskFactors Value'
riskFactors _ _ = RiskFactors
  { o_rf_CURS: fromInt 1
  , o_rf_RRMO: fromInt 1
  , o_rf_SCMO: fromInt 1
  , pp_payoff: fromInt 0
  }
  where
  fromInt = Constant' <<< BigInt.fromInt <<< (marloweFixedPoint * _)
