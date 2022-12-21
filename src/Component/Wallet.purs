module Component.Wallet where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import Web.HTML (HTMLDocument, window)

data WalletState
  = -- { cardano :: Maybe Wallet.Cardano }
    Connected
  | NotConnected

mkWalletConnect :: Effect (WalletState -> JSX)
mkWalletConnect = do
  component "Wallet" \walletState -> React.do
    ((state :: WalletState) /\ updateState) <- useState walletState
    let
      onConnectClick = handler_ do
        updateState f
      f Connected = NotConnected
      f NotConnected = Connected

    case state of
      Connected -> pure $
        DOM.div {}
          [ DOM.button
              { className: "btn btn-primary", onClick: onConnectClick }
              "Disconnect Wallet"
          ]
      NotConnected -> pure $
        DOM.div {}
          [ DOM.button
              { className: "btn btn-primary", onClick: onConnectClick }
              "Connect Wallet"
          ]

connectWallet :: Effect Unit
connectWallet = launchAff_ do
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
