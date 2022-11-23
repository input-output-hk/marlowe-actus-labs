module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Class.Console (log)
import Test.Actus.Domain.ContractTerms as ContractTerms
import Test.Spec (pending, describe, it)
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec, runSpec')


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ]
  do
    Spec.parallel do
      ContractTerms.spec
