module Main
  ( main
  ) where

import Prelude

import Component.App (mkApp)
import Component.ConnectWallet (mkConnectWallet)
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Contrib.Data.Argonaut (JsonParser)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (error, throw)
import Marlowe.Runtime.Web.Client (foldMapMPages, foldMapMPages', getPage')
import Marlowe.Runtime.Web.Types (ResourceLink(..), ServerURL(..), api)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, useState)
import Wallet as Wallet
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

liftEitherWith :: forall a err. (err -> String) -> Either err a -> Effect a
liftEitherWith showErr = either (throw <<< showErr) pure

liftEither :: forall a err. Show err => Either err a -> Effect a
liftEither = either (throw <<< show) pure

type Config =
  { marloweWebServerUrl :: ServerURL
  , develMode :: Boolean
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  pure { marloweWebServerUrl: ServerURL marloweWebServerUrl, develMode }


main :: Json -> Effect Unit
main configJson = do
  config <- liftEither $ decodeConfig configJson
  let
    logger :: String -> Effect Unit
    logger =
      if config.develMode then Console.log
      else const (pure unit)

  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      launchAff_ do
        -- contracts <- foldMapMPages' config.marloweWebServerUrl api (pure <<< _.page) >>= liftEither >>> liftEffect
        -- FIXME: this is a temporary hack to get the first page of contracts to speed up development
        contracts <- getPage' config.marloweWebServerUrl api Nothing >>= liftEither >>> liftEffect <#> _.page
        walletInfoCtx <- liftEffect $ createContext Nothing
        let
          mkAppCtx =
            { walletInfoCtx
            , logger
            , contracts
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ app unit
