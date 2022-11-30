module Test.Marlowe.Web.ContractHeader where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, fromObject, fromString, jsonParser, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Effect.Exception (error)
import Foreign.Object as Object
import Marlowe.Runtime.Web (ContractHeader, ContractState(..))
import Marlowe.Runtime.Web.Types (ResourceLink, ResourceWithLinks, decodeResourceWithLink)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "ContractHeader" do
    describe "decodeJson" do
      it "contract-headers.json" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/contract-headers.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        (contractsWithLinksJson :: Array Json) <- either (throwError <<< error <<< show) pure do
            obj <- decodeJson json
            obj .: "results"

        for_ contractsWithLinksJson \contractWithLinksJson -> do
          let
            contracts :: Either JsonDecodeError (ResourceWithLinks ContractHeader (contract :: ResourceLink))
            contracts = decodeResourceWithLink (map decodeJson) contractWithLinksJson
          case contracts of
            Left err -> do
              let
                errJson = fromObject $ Object.fromHomogeneous { json: contractWithLinksJson, err: fromString $ show err }
              fail $ stringify errJson
            Right _ -> pure unit

      it "contract-state.json" do
        jsonStr <- readTextFile UTF8 "./test/Marlowe/Runtime/Web/contract-state.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        let
          contractState :: Either JsonDecodeError (ResourceWithLinks ContractState (transactions :: ResourceLink))
          contractState = decodeResourceWithLink (map decodeJson) json
        case contractState of
          Left err -> do
            let
              errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
            fail $ stringify errJson
          Right _ -> pure unit

      pending "feature complete"
