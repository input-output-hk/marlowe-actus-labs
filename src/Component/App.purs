module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.Types (ContractHeaderResource, MkComponentM, WalletInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.React.Bootstrap.Icons as Icons
import Control.Monad.Reader.Class (asks)
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype as Newtype
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet
import Web.HTML (window)

-- | Debugging helpers which allow us to automatically connect wallet
data WalletBrand
  = Yoroi
  | Nami
  | Eternl
instance Show WalletBrand where
  show Yoroi = "Yoroi"
  show Nami = "Nami"
  show Eternl = "Eternl"

autoConnectWallet :: WalletBrand -> (WalletInfo Wallet.Api -> Effect Unit) -> Aff Unit
autoConnectWallet walletBrand onSuccess = liftEffect (window >>= Wallet.cardano) >>= case _ of
  Nothing -> do
    liftEffect $ throw $ "Missing \"cardano\" window attr"
  Just cardano -> do
    let
      extractWallet = case walletBrand of
        Nami -> Wallet.nami
        Yoroi -> Wallet.yoroi
        Eternl -> Wallet.eternl
    liftEffect (extractWallet cardano) >>= traverse walletInfo >>= case _ of
      Nothing -> do
        liftEffect $ throw $ "Unable to extract wallet " <> show walletBrand
      Just walletInfo@(WalletInfo { wallet }) -> do
        walletApi <- Wallet.enable wallet
        let
          walletInfo' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) walletInfo
        liftEffect $ onSuccess walletInfo'

mkApp :: MkComponentM (Unit -> JSX)
mkApp = do
  contractListComponent <- mkContractList
  eventListComponent <- mkEventList
  connectWallet <- mkConnectWallet

  walletInfoCtx <- asks _.walletInfoCtx

  -- FIXME: This gonna be replaced by a contract event emitter
  (contracts :: Array ContractHeaderResource) <- asks _.contracts

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    configuringWallet /\ setConfiguringWallet <- useState' false

    let
      debugWallet = Just Nami
    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo

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
          , DOM.div { className: "col" } $ eventListComponent { contractList: contracts, connectedWallet: possibleWalletInfo }
          ]
      ]

