module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.Types (ContractEvent, MkComponentM, WalletInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.React.Bootstrap.Icons as Icons
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Halogen.Subscription as Subscription
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, useState')
import React.Basic.Hooks as React

mkApp :: MkComponentM (Unit -> JSX)
mkApp = do
  runtime <- asks _.runtime
  contractListComponent <- mkContractList
  eventListComponent <- liftEffect $ mkEventList runtime
  connectWallet <- mkConnectWallet

  walletInfoCtx <- asks _.walletInfoCtx

  -- FIXME: This gonna be replaced by a contract event emitter
  (contractEmitter :: Subscription.Emitter ContractEvent) <- asks _.contractEmitter
  let contracts = []

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    configuringWallet /\ setConfiguringWallet <- useState' false

    pure $ provider walletInfoCtx possibleWalletInfo
      [ DOM.nav { className: "navbar mb-lg-4 navbar-expand-sm navbar-light bg-light py-0" } $
          DOM.div { className: "container-xl" }
            [ DOM.a { href: "#", className: "navbar-brand" } "Marlowe Actus Labs"
            , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                [ DOM.ul { className: "navbar-nav gap-2" }
                    [ DOM.li { className: "nav-item" } $
                        linkWithIcon
                          { icon: Icons.infoSquare
                          , label: (DOOM.text "About")
                          , extraClassNames: "nav-link"
                          , onClick: (pure unit)
                          }
                    , DOM.li { className: "nav-item" } $
                        linkWithIcon
                          { icon: Icons.cashStack
                          , label: DOOM.text "Cash flows"
                          , extraClassNames: "nav-link"
                          , onClick: pure unit
                          }
                    , DOM.li { className: "nav-item" } $
                        case possibleWalletInfo of
                          Just (WalletInfo wallet) -> link
                            { label: DOOM.span_
                                [ DOOM.img { src: wallet.icon, alt: wallet.name, className: "w-1_2rem me-1" }
                                , DOOM.span_ [ DOOM.text $ wallet.name <> " wallet" ]
                                ]
                            , extraClassNames: "nav-link"
                            , onClick: setConfiguringWallet true
                            }
                          Nothing -> linkWithIcon
                            { icon: Icons.wallet2
                            , label: DOOM.text "Connect Wallet"
                            , extraClassNames: "nav-link"
                            , onClick: setConfiguringWallet true
                            }
                    ]
                ] <> Monoid.guard configuringWallet do
                  let
                    jsx = connectWallet
                      { currentlyConnected: possibleWalletInfo
                      , onWalletConnect: \result -> do
                          case result of
                            ConnectWallet.Connected walletInfo -> do
                              setWalletInfo (Just walletInfo)
                            ConnectWallet.ConnectionError _ -> pure unit
                            ConnectWallet.NoWallets -> pure unit
                          setConfiguringWallet false

                      , onDismiss: setConfiguringWallet false
                      , inModal: true
                      }
                  [ jsx ]
            ]
      , DOM.div { className: "container-xl" } $
          [ contractListComponent { contractList: contracts, connectedWallet: possibleWalletInfo }
          , DOM.div { className: "col" } $ eventListComponent contracts
          ]
      ]

