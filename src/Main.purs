module Main
  ( main
  ) where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.App (mkApp)
import Component.Types (ContractEvent(..))
import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Map as Contrib.Map
import Control.Monad.Reader (runReaderT)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype as Newtype
import Data.Traversable (for_)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Timer as Timer
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client as Client
import Marlowe.Runtime.Web.Types (ContractHeader, ServerURL(..), TxOutRef, api)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

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

type IntervalEvent = Unit

withSetIntervalEmitter :: forall a. Int -> (Subscription.Emitter IntervalEvent -> Aff a) -> Aff a
withSetIntervalEmitter interval k = do
  { emitter, listener } <- liftEffect Subscription.create
  let
    aquire :: Aff Timer.IntervalId
    aquire = liftEffect $ Timer.setInterval interval (Subscription.notify listener unit)

    cleanup :: Timer.IntervalId -> Aff Unit
    cleanup = liftEffect <<< Timer.clearInterval
  Aff.bracket aquire cleanup \_ -> k emitter

type GetContractsEvent =
  { additions :: Map TxOutRef ContractHeader
  , deletions :: Map TxOutRef ContractHeader
  , updates :: Map TxOutRef { old :: ContractHeader, new :: ContractHeader }
  }

contractScanner :: Map TxOutRef ContractHeader -> Map TxOutRef ContractHeader /\ GetContractsEvent -> Map TxOutRef ContractHeader /\ GetContractsEvent
contractScanner new (old /\ _) =
  new /\
    { additions: Contrib.Map.additions old new
    , deletions: Contrib.Map.deletions old new
    , updates: Contrib.Map.updates old new
    }

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
      launchAff_ $ withSetIntervalEmitter 1_000 \setIntervalEmitter -> do
        initialContracts :: Array ContractHeader <- liftEffect <<< liftEither =<< Client.getContracts' config.marloweWebServerUrl api Nothing
        { emitter: contractsEmitter, listener: contractsListener :: Subscription.Listener (Map TxOutRef ContractHeader) } <- liftEffect Subscription.create

        contractsSubscription <- liftEffect $ Subscription.subscribe setIntervalEmitter \_ -> launchAff_ do
          liftEffect <<< (Subscription.notify contractsListener <<< Contrib.Map.fromFoldableBy (_.contractId <<< Newtype.unwrap) <=< liftEither)
            =<< Client.getContracts' config.marloweWebServerUrl api Nothing

        let
          getContractsEvents :: Subscription.Emitter GetContractsEvent
          getContractsEvents = map snd $ Subscription.fold contractScanner contractsEmitter $
            Contrib.Map.fromFoldableBy (_.contractId <<< Newtype.unwrap) initialContracts
              /\ { additions: Map.empty, deletions: Map.empty, updates: Map.empty }

        { emitter: contractEmitter, listener: contractListener :: Subscription.Listener ContractEvent } <- liftEffect Subscription.create

        contractSubscription <- liftEffect $ Subscription.subscribe getContractsEvents \e -> do
          for_ e.additions $ Subscription.notify contractListener <<< Addition
          for_ e.deletions $ Subscription.notify contractListener <<< Deletion
          for_ e.updates $ Subscription.notify contractListener <<< Update

        CardanoMultiplatformLib.importLib >>= case _ of
          Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
          Just cardanoMultiplatformLib -> do
            walletInfoCtx <- liftEffect $ createContext Nothing
            let
              mkAppCtx =
                { cardanoMultiplatformLib
                , walletInfoCtx
                , logger
                , contractEvents: contractEmitter
                , runtime
                }

            app <- liftEffect $ runReaderT mkApp mkAppCtx
            liftEffect $ renderRoot reactRoot $ app unit

        -- FIXME: We need a resource monad to clean this stuff up and make cleanup reliable:
        liftEffect do
          Subscription.unsubscribe contractsSubscription
          Subscription.unsubscribe contractSubscription
