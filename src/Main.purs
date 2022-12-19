module Main
  ( main
  ) where

import Prelude

import Component.ContractList (mkContractList)
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Wallet as Wallet
import Web.HTML (window)
import Effect.Aff (launchAff_)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import Marlowe.Runtime.Web.Client (foldMapMPages, foldMapMPages')
import Marlowe.Runtime.Web.Types (ResourceLink(..), ServerURL(..), api)
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
serverUrl = ServerURL "http://localhost:49706"

liftEitherWith :: forall a err. (err -> String) -> Either err a -> Effect a
liftEitherWith showErr = either (throw <<< showErr) pure

liftEither :: forall a err. Show err => Either err a -> Effect a
liftEither = either (throw <<< show) pure

main :: Effect Unit
main = do
  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      contractListComponent <- mkContractList
      launchAff_ do
        -- contracts <- foldMapMPages' serverUrl api (pure <<< _.page) >>= liftEither >>> liftEffect
        liftEffect $ renderRoot reactRoot (contractListComponent [])

