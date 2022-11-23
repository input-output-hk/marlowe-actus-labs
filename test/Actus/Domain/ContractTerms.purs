module Test.Actus.Domain.ContractTerms where

import Prelude

import Actus.Domain (ContractTerms)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, fromString, jsonParser)
import Data.Argonaut as A
import Data.Argonaut.Decode ((.:))
import Data.Bifunctor (bimap)
import Data.Decimal (Decimal)
import Data.Either (Either(..), either)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Error)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (cwd)
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)

spec = do
  describe "Actus.Domain.ContractTerms" do
    describe "decodeJson" do
      it "acuts-tests-lam.json" do
        jsonStr <- readTextFile UTF8 "./test/Actus/Domain/actus-tests-lam.json"
        json <- either (throwError <<< error) pure $ jsonParser jsonStr

        (fixtures :: Object Json) <- either (throwError <<< error <<< show) pure do
          obj <- decodeJson json
          forWithIndex obj \lamId lamJson -> do
            obj <- decodeJson lamJson
            termsJson <- obj .: "terms"
            pure $ termsJson

        forWithIndex_ fixtures \lamId termsJson -> do
          let
            (terms :: Either JsonDecodeError (ContractTerms Decimal)) = decodeJson termsJson
          case terms of
            Left err -> fail (lamId <> ": " <> show err)
            Right _ -> pure unit

      pending "feature complete"
