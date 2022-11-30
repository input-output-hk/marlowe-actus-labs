module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Actus.Domain.ContractTerms as ContractTerms
import Test.Marlowe.Runtime.Web as Web
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ]
  do
    Spec.parallel do
      ContractTerms.spec
      Web.spec
