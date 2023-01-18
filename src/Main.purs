module Main
  ( main
  , testWallet
  ) where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Component.Types (ContractHeaderResource)
import Contrib.Data.Argonaut (JsonParser)
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Array (filter)
import Data.Either (Either, either)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(Milliseconds), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client (getPage')
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata(..), ServerURL(..), api)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
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
            Console.log <<< ("getBalance: " <> _) <<< unsafeStringify =<< Wallet.getBalance api
            Console.log <<< ("getChangeAddress: " <> _) <<< unsafeStringify =<< Wallet.getChangeAddress api
            Console.log <<< ("getRewardAddresses: " <> _) <<< unsafeStringify =<< Wallet.getRewardAddresses api
            Console.log <<< ("getUnusedAddresses: " <> _) <<< unsafeStringify =<< Wallet.getUnusedAddresses api
            Console.log <<< ("getUsedAddresses: " <> _) <<< unsafeStringify =<< Wallet.getUsedAddresses api
            Console.log <<< ("getUtxos: " <> _) <<< unsafeStringify =<< Wallet.getUtxos api

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
    runtime = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl

  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      launchAff_ do
        -- contracts <- foldMapMPages' config.marloweWebServerUrl api (pure <<< _.page) >>= liftEither >>> liftEffect
        -- FIXME: this is a temporary hack to get the first page of contracts to speed up development
        let
          isActus :: ContractHeaderResource -> Boolean
          isActus { resource: ContractHeader { metadata: Metadata md } } = isJust $ lookup actusMetadataKey md
        contracts <- filter isActus <$> (getPage' config.marloweWebServerUrl api Nothing >>= liftEither >>> liftEffect <#> _.page)
        -- let contracts = []

        CardanoMultiplatformLib.importLib >>= case _ of
          Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
          Just cardanoMultiplatformLib -> do
            walletInfoCtx <- liftEffect $ createContext Nothing
            msgHubComponent /\ msgHub <- liftEffect $ mkMessageHub
            let
              mkAppCtx =
                { cardanoMultiplatformLib
                , walletInfoCtx
                , logger
                , contracts
                , runtime
                , msgHub
                }

            app <- liftEffect $ runReaderT mkApp mkAppCtx
            liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]
