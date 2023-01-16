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
import Control.Monad.Rec.Loops (whileM_)
import Data.Argonaut (Json, decodeJson, (.:))
import Data.Either (Either, either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype as Newtype
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Fiber, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web as Marlowe.Runtime.Web
import Marlowe.Runtime.Web.Client (foldMapMContractPages)
import Marlowe.Runtime.Web.Client as Client
import Marlowe.Runtime.Web.Types (ContractHeader, ServerURL(..), TxOutRef, api)
import React.Basic (createContext)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

liftEither :: forall a m err. MonadEffect m => Show err => Either err a -> m a
liftEither = either (liftEffect <<< throw <<< show) pure

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

contractsById :: Array ContractHeader -> Map TxOutRef ContractHeader
contractsById = Contrib.Map.fromFoldableBy $ _.contractId <<< Newtype.unwrap

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

    contracts :: Ref (Map TxOutRef ContractHeader) <-
      liftEffect <<< Ref.new =<< contractsById <$> (liftEither =<< Client.getContracts' config.marloweWebServerUrl api Nothing)

    { emitter: contractEmitter, listener: contractListener :: Subscription.Listener ContractEvent } <-
      liftEffect Subscription.create

    _ :: Fiber Unit <- forkAff $ whileM_ (pure true) do
      previousContracts <- liftEffect $ Ref.read contracts
      nextContracts :: Map TxOutRef ContractHeader <-
        map contractsById $ liftEither =<< foldMapMContractPages config.marloweWebServerUrl api Nothing \pageContracts -> do
          liftEffect do
            let
              cs :: Map TxOutRef ContractHeader
              cs = contractsById pageContracts
            Ref.modify_ (Map.union cs) contracts
            for_ (Contrib.Map.additions previousContracts cs) $ Subscription.notify contractListener <<< Addition
            for_ (Contrib.Map.updates previousContracts cs) $ Subscription.notify contractListener <<< Update
          pure pageContracts
      liftEffect do
        Ref.write nextContracts contracts
        for_ (Contrib.Map.deletions previousContracts nextContracts) $ Subscription.notify contractListener <<< Deletion
      delay $ Milliseconds 1_000.0

    CardanoMultiplatformLib.importLib >>= case _ of
      Nothing -> liftEffect $ logger "Cardano serialization lib loading failed"
      Just cardanoMultiplatformLib -> do
        walletInfoCtx <- liftEffect $ createContext Nothing
        let
          mkAppCtx =
            { cardanoMultiplatformLib
            , walletInfoCtx
            , logger
            , contractEmitter
            , getContracts: Ref.read contracts
            , runtime
            }

        app <- liftEffect $ runReaderT mkApp mkAppCtx
        liftEffect $ renderRoot reactRoot $ app unit
