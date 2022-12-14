module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Wallet as Wallet
import Web.HTML (window)

main :: Effect Unit
main = do
  w <- window
  mC <- Wallet.cardano w
  case mC of
    Nothing -> Console.log "nay"
    Just c -> launchAff_ do
      delay (Milliseconds 3_000.0)
      liftEffect (Wallet.nami c)
        >>= case _ of
          Nothing -> Console.log "boo"
          Just nami -> do
            api <- Wallet.enable nami
            Console.log <<< show =<< Wallet.getNetworkId api
