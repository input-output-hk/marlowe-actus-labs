module Main
  ( main
  , testWallet
  ) where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.MessageHub (mkMessageHub)
import Component.Types (ContractEvent(..))
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Map as Contrib.Map
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
import Marlowe.Runtime.Web.Types (GetContractsResponse, ServerURL(..), TxOutRef, api)
import Prim.TypeError (class Warn, Text)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Wallet as Wallet
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

liftEither :: forall a m err. MonadEffect m => Show err => Either err a -> m a
liftEither = either (liftEffect <<< throw <<< show) pure

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

contractsById :: Array GetContractsResponse -> Map TxOutRef GetContractsResponse
contractsById = Contrib.Map.fromFoldableBy $ _.contractId <<< Newtype.unwrap <<< _.resource

pushPullContractsStreams
  :: Warn (Text "pushPullContractsStreams is deprecated, use web socket based implementation instead!")
  => Milliseconds
  -> ServerURL
  -> Aff { contractEmitter :: Subscription.Emitter ContractEvent, getContracts :: Effect (Map TxOutRef GetContractsResponse) }
pushPullContractsStreams waitBetween serverUrl = do
  contracts :: Ref (Map TxOutRef GetContractsResponse) <- liftEffect $ Ref.new Map.empty

  { emitter: contractEmitter, listener: contractListener :: Subscription.Listener ContractEvent } <-
    liftEffect Subscription.create

  _ :: Fiber Unit <- forkAff $ whileM_ (pure true) do
    previousContracts <- liftEffect $ Ref.read contracts
    nextContracts :: Map TxOutRef GetContractsResponse <-
      map contractsById $ liftEither =<< foldMapMContractPages serverUrl api Nothing \pageContracts -> do
        liftEffect do
          let
            cs :: Map TxOutRef GetContractsResponse
            cs = contractsById pageContracts
          Ref.modify_ (Map.union cs) contracts
          for_ (Contrib.Map.additions previousContracts cs) $ Subscription.notify contractListener <<< Addition
          for_ (Contrib.Map.updates previousContracts cs) $ Subscription.notify contractListener <<< Update
        delay (Milliseconds 500.0)
        pure pageContracts
    liftEffect do
      Ref.write nextContracts contracts
      for_ (Contrib.Map.deletions previousContracts nextContracts) $ Subscription.notify contractListener <<< Deletion
    delay waitBetween

  pure { contractEmitter, getContracts: Ref.read contracts }

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
  container :: Element <- maybe (throw "Could not find element with id 'app-root'") pure =<<
    (getElementById "app-root" $ toNonElementParentNode doc)
  reactRoot <- createRoot container
  launchAff_ do
    { contractEmitter, getContracts } <- pushPullContractsStreams (Milliseconds 1_000.0) config.marloweWebServerUrl

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
            , contractEmitter
            , getContracts
            , msgHub
            , runtime
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ msgHubComponent [ app unit ]
