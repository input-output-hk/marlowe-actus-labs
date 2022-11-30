module Test.Marlowe.Runtime.Web where

import Prelude

import Test.Marlowe.Web.ContractHeader as ContractHeader
import Test.Spec (Spec, describe)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  describe "Marlowe.Web" $ Spec.parallel do
   ContractHeader.spec
