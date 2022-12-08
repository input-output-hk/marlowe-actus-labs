module Test.Wallet
  ( spec
  ) where

import Prelude

import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  Spec.describe "Wallet" do
    Spec.it "fails" do
      5 `shouldEqual` 4
