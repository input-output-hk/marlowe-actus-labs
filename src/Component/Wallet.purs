module Component.ConnectWallet where

import Prelude

import Component.Types (MkComponentM, WalletInfo)
import Component.Widgets (spinner)
import Component.Widgets.Form (mkSingleChoiceField)
import Component.Widgets.Form as Form
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Foldable (length)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Type.Prelude (Proxy(..))
import Wallet (Wallet)
import Wallet as Wallet
import Web.HTML (window)


type Wallets = Map String Wallet

walletInfo :: Wallet -> Aff WalletInfo
walletInfo wallet =
  { name: _, icon: _, isEnabled: _, apiVersion: _, wallet }
  <$> liftEffect (Wallet.name wallet)
  <*> liftEffect (Wallet.icon wallet)
  <*> Wallet.isEnabled wallet
  <*> liftEffect (Wallet.apiVersion wallet)

data Response
  = NoWallets
  | Aborted
  | Connected WalletInfo
  | Disconnected WalletInfo

type Props =
  { currentlyConnected :: Maybe WalletInfo
  , onWalletConnected :: Response -> Effect Unit
  }

-- mkComponent :: Effect (WalletState -> JSX)
mkConnectWallet :: MkComponentM (Props -> JSX)
mkConnectWallet = do
  singleChoiceField <- liftEffect mkSingleChoiceField
  logger <- asks _.logger

  liftEffect $ component "Wallet" \{ currentlyConnected, onWalletConnected } -> React.do
    possibleWallets /\ setWallets <- useState' (Nothing :: Maybe (ArrayAL 1 WalletInfo))
    selectedWallet /\ setSelectedWallet <- useState' (Nothing :: Maybe WalletInfo)

    useEffectOnce do
      liftEffect (Wallet.cardano =<< window) >>= case _ of
        Nothing -> pure unit
        Just cardano -> launchAff_ do
          (nami :: Maybe WalletInfo) <- liftEffect (Wallet.nami cardano) >>= traverse walletInfo
          yoroi <- liftEffect (Wallet.yoroi cardano) >>= traverse walletInfo
          case ArrayAL.fromArray (Proxy :: Proxy 1) (Array.catMaybes [nami, yoroi]) of
            Nothing -> liftEffect $ onWalletConnected NoWallets
            Just wallets -> liftEffect $ setWallets (Just wallets)
      pure (pure unit)

    pure $ case possibleWallets of
      Nothing -> spinner Nothing
      Just wallets -> do
        let
          { head, tail } = ArrayAL.uncons wallets
          choices = wallets <#> \wallet ->
            Form.choice wallet.name $ DOM.span {}
              [ DOOM.img { src: wallet.icon, alt: wallet.name }
              , DOOM.span_ [ DOOM.text wallet.name ]
              ]

        singleChoiceField
          { initialValue: head.name
          , onValueChange: \walletName ->
              case Array.find (\wallet -> wallet.name == walletName) (ArrayAL.toArray wallets) of
                Nothing -> do
                  logger "INVALID WALLET NAME"
                  pure unit
                Just walletInfo -> setSelectedWallet (Just walletInfo)
          , type: Form.RadioButtonField choices
          }

        -- { heade -> connectWallet (Array.head wallets) onWalletConnected


-- case Array.uncons <$> possibleWallets of
--   Just { head, tail } -> do
--     let
--       walletNames = Map.keys wallets
--       wallet = Map.lookup currentlyConnected wallets
--     singleChoiceField 
--       { initialValue: ""
--       , 

