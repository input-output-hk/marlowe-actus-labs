module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.Modal (mkModal)
import Component.Types (MkComponentM, ContractHeaderResource)
import Component.Widgets (linkButtonWithIcon, linkWithIcon)
import Contrib.React.Bootstrap as Bootstrap
import Contrib.React.Bootstrap.Icons as Icons
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

    pure $ provider walletInfoCtx possibleWalletInfo
        [ DOM.nav { className: "navbar mb-lg-4 navbar-expand-sm navbar-light" } $
            DOM.div { className: "container-xl" }
              [ DOM.a { href: "#", className: "navbar-brand" } "Marlowe Actus Labs"
              , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                  [ DOM.ul { className: "navbar-nav gap-2" }
                    [ DOM.li { className: "nav-item" } $
                        linkWithIcon Icons.infoSquare (DOOM.text "About") "nav-link" (pure unit)
                    , DOM.li { className: "nav-item" } $
                        linkWithIcon Icons.cashStack (DOOM.text "Cash flows") "nav-link" (pure unit)
                    , DOM.li { className: "nav-item" } $
                        linkWithIcon Icons.wallet2 (DOOM.text "Connect Wallet") "nav-link" (setConfiguringWallet true)
                    ]
                  ] <> Monoid.guard configuringWallet
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
              [ contractListComponent contracts
              , DOM.div { className: "col" } $ eventListComponent contracts
              ]
        ]

