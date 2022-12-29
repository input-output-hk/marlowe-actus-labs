module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.Modal (mkModal)
import Component.Types (MkComponentM, ContractHeaderResource)
import Component.Widgets (linkButtonWithIcon, linkWithIcon)
import Component.Widgets.Icons as Icons
import Component.Widgets.Icons as Icons
import Contrib.React.Bootstrap as Bootstrap
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, useState, useState')
import React.Basic.Hooks as React

mkApp :: MkComponentM (Unit -> JSX)
mkApp = do
  contractListComponent <- liftEffect mkContractList
  eventListComponent <- liftEffect mkEventList
  walletComponent <- mkConnectWallet

  walletInfoCtx <- asks _.walletInfoCtx
  modal <- liftEffect $ mkModal

  -- FIXME: This gonna be replaced by a contract event emitter
  (contracts :: Array ContractHeaderResource) <- asks _.contracts

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    configuringWallet /\ setConfiguringWallet <- useState' false

    pure $ provider walletInfoCtx possibleWalletInfo $
        [ DOM.div { className: "container-xl" } $
            DOM.div { className: "row" } $
              [ Bootstrap.alert $ { dissmisable: true, show: true, closeLabel: "Close", variant: Bootstrap.variant.dark, children: _ }
                [ DOOM.text "ALERT BODY"
                ]
              , DOM.a { href: "#", className: "navbar-brand" } "ACTUS 1"
              , DOM.div { className: "col-3" } $ "About"
              , DOM.div { className: "col-3 text-end" } $
                  [ linkButtonWithIcon Icons.wallet2 (DOOM.text "Connect Wallet") $ setConfiguringWallet true
                  ]
                  <> Monoid.guard configuringWallet
                    [ modal
                      { onDismiss: setConfiguringWallet false
                      , body: walletComponent
                        { currentlyConnected: Nothing
                        , onWalletConnected: \_ -> pure unit
                        }
                      , title: DOOM.text "Configuring wallet"
                      }
                    ]
              ]
        , DOM.div { className: "container-xl" } $ Array.singleton $
            DOM.div { className: "row" } $
              [ DOM.div { className: "col" } $ contractListComponent contracts
              , DOM.div { className: "col" } $ eventListComponent contracts
              ]
        ]

