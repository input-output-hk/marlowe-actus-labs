module Test.Main where

import Prelude

import Actus.TestFramework (TestCase, decodeTestCase)
import Actus.TestFramework as TestFramework
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, jsonParser)
import Data.Either (Either, either)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (error)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Test.Actus.Domain.ContractTerms as ContractTerms
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ do

  testsPAM <- readFile "./test/Actus/Domain/actus-tests-pam.json"
  testsLAM <- readFile "./test/Actus/Domain/actus-tests-lam.json"
  testsNAM <- readFile "./test/Actus/Domain/actus-tests-nam.json"
  testsANN <- readFile "./test/Actus/Domain/actus-tests-ann.json"

  runSpec [ consoleReporter ] $ do
    Spec.parallel do
      ContractTerms.spec
      Web.spec
      TestFramework.spec testsPAM
      TestFramework.spec testsLAM
      TestFramework.spec testsNAM
      -- TestFramework.spec testsANN

  where
  readFile file = do
    jsonStr <- readTextFile UTF8 file
    json <- either (throwError <<< error) pure $ jsonParser jsonStr

    (fixtures :: Object Json) <- either (throwError <<< error <<< show) pure do
      decodeJson json >>= traverse \pamJson -> do
        obj <- decodeJson pamJson
        pure $ obj

    forWithIndex fixtures \testId testCase -> do
      let (tc :: Either JsonDecodeError TestCase) = decodeTestCase testCase
      pure (testId /\ tc)
