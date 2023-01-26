module Main where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (transaction)
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Map as Contrib.Map
import Contrib.Effect as Effect
import Control.Monad.Reader (runReaderT)
import Control.Monad.Rec.Loops (whileM_)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as Newtype
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription as Subscription
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client (foldMapMContractPages)
import Marlowe.Runtime.Web.Client (getPage')
import Marlowe.Runtime.Web.Streaming (ContractStream(..), PollingInterval(..), RequestInterval(..))
import Marlowe.Runtime.Web.Streaming as Streaming
import Marlowe.Runtime.Web.Types (GetContractsResponse, ServerURL(..), TxOutRef, api)
import Marlowe.Runtime.Web.Types (Metadata(..), ServerURL(..), GetContractsResponse, TxOutRef, api)
import Prim.TypeError (class Warn, Text)
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
            api <- Wallet.enable_ nami
            Console.log <<< ("getBalance: " <> _) <<< unsafeStringify =<< Wallet.getBalance api
            Console.log <<< ("getChangeAddress: " <> _) <<< unsafeStringify =<< Wallet.getChangeAddress api
            Console.log <<< ("getRewardAddresses: " <> _) <<< unsafeStringify =<< Wallet.getRewardAddresses api
            Console.log <<< ("getUnusedAddresses: " <> _) <<< unsafeStringify =<< Wallet.getUnusedAddresses api
            Console.log <<< ("getUsedAddresses: " <> _) <<< unsafeStringify =<< Wallet.getUsedAddresses api
            Console.log <<< ("getUtxos: " <> _) <<< unsafeStringify =<< Wallet.getUtxos api

type Config =
  { marloweWebServerUrl :: ServerURL
  , develMode :: Boolean
  , aboutMarkdown :: String
  }

decodeConfig :: JsonParser Config
decodeConfig json = do
  obj <- decodeJson json
  marloweWebServerUrl <- obj .: "marloweWebServerUrl"
  develMode <- obj .: "develMode"
  aboutMarkdown <- obj .: "aboutMarkdown"
  pure { marloweWebServerUrl: ServerURL marloweWebServerUrl, develMode, aboutMarkdown }

main :: Json -> Effect Unit
main configJson = do
  config <- Effect.liftEither $ decodeConfig configJson

  let
    logger :: String -> Effect Unit
    logger =
      if config.develMode then Console.log
      else const (pure unit)
    runtime = Marlowe.Runtime.Web.runtime config.marloweWebServerUrl

  doc :: HTMLDocument <- document =<< window
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do
    let
      reqInterval = RequestInterval (Milliseconds 50.0)
      pollInterval = PollingInterval (Milliseconds 20_000.0)

    contractStream <- Streaming.mkContractsWithTransactions pollInterval reqInterval config.marloweWebServerUrl

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
            , contractStream
            , msgHub
            , runtime
            , aboutMarkdown: config.aboutMarkdown
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]
