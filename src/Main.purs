module Main
  ( main
  ) where

import Prelude

import Component.ContractList (contractList)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Wallet as Wallet
import Web.HTML (window)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Marlowe.Runtime.Web.Client (fetchContractHeaders)
import Marlowe.Runtime.Web.Types (ServerURL(..))
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


-- | TODO: move this testing code to a separate "app"
testWallet :: Effect Unit
testWallet = launchAff_ do
  delay (Milliseconds 3_000.0)
  mC <- liftEffect (Wallet.cardano =<< window)
  case mC of
    Nothing -> Console.log "nay"
    Just c -> do
      liftEffect (Wallet.nami c)
        >>= case _ of
          Nothing -> Console.log "boo"
          Just nami -> do
            api <- Wallet.enable nami
            Console.log <<< ("getBalance: " <> _) <<< show =<< Wallet.getBalance api
            Console.log <<< ("getChangeAddress: " <> _) <<< show =<< Wallet.getChangeAddress api
            Console.log <<< ("getRewardAddresses: " <> _) <<< show =<< Wallet.getRewardAddresses api
            Console.log <<< ("getUnusedAddresses: " <> _) <<< show =<< Wallet.getUnusedAddresses api
            Console.log <<< ("getUsedAddresses: " <> _) <<< show =<< Wallet.getUsedAddresses api
            Console.log <<< ("getUtxos: " <> _) <<< show =<< Wallet.getUtxos api

serverUrl :: ServerURL
serverUrl = ServerURL "http://localhost:49207" -- TODO: to config

main :: Effect Unit
main = do
  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container

      launchAff_ do
        contracts <- fetchContractHeaders serverUrl ("/contracts/")
        liftEffect $ renderRoot reactRoot (contractList contracts)
