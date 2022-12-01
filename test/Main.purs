module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ServerURL(..))
import Node.Process (getEnv)
import Test.Actus.Domain.ContractTerms as ContractTerms
import Test.Marlowe.Runtime.Web (_MARLOWE_WEB_SERVER_URL)
import Test.Marlowe.Runtime.Web as Web
import Test.Spec as Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  serverUrlStr <- getEnv <#> Object.lookup _MARLOWE_WEB_SERVER_URL >>> map ServerURL
  launchAff_ $ do
    runSpec [ consoleReporter ] do
      Spec.parallel do
        ContractTerms.spec
        Web.spec serverUrlStr
