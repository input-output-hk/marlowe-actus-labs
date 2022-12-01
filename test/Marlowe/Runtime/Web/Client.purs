module Test.Marlowe.Web.Client where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, fromObject, fromString, jsonParser, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Effect.Exception (error)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink, ResourceWithLinks, ServerURL(..), Tx, TxHeader, decodeResourceWithLink)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: ServerURL -> Spec Unit
spec serverUrl@(ServerURL serverUrlStr) = do
  describe ("Testing web client against" <> serverUrlStr) do
     it "STUB" do
        pure unit
