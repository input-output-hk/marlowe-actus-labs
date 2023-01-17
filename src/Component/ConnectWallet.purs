module Component.ConnectWallet where

import Prelude

import Component.Modal (mkModal)
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link, spinner)
import Component.Widgets.Form (mkSingleChoiceField)
import Component.Widgets.Form as Form
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (button, h2_, img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet (Wallet)
import Wallet as Wallet
import Web.HTML (window)

type Wallets = Map String Wallet

walletInfo :: Wallet -> Aff (WalletInfo Wallet)
walletInfo wallet = WalletInfo <$> ado
  name <- liftEffect (Wallet.name wallet)
  icon <- liftEffect (Wallet.icon wallet)
  isEnabled <- Wallet.isEnabled wallet
  apiVersion <- liftEffect (Wallet.apiVersion wallet)
  in
    { name, icon, isEnabled, apiVersion, wallet }

data Response
  = NoWallets
  | ConnectionError Error
  | Connected (WalletInfo Wallet.Api)

type Props =
  { currentlyConnected :: Maybe (WalletInfo Wallet.Api)
  , onDismiss :: Effect Unit
  , onWalletConnect :: Response -> Effect Unit
  , inModal :: Boolean
  }

mkConnectWallet :: MkComponentM (Props -> JSX)
mkConnectWallet = do
  singleChoiceField <- liftEffect mkSingleChoiceField
  modal <- liftEffect mkModal

  liftEffect $ component "Wallet" \{ currentlyConnected, onWalletConnect, onDismiss, inModal } -> React.do
    -- pure \{ currentlyConnected, onWalletConnect, onDismiss } -> coerceHook React.do
    possibleWallets /\ setWallets <- useState' (Nothing :: Maybe (ArrayAL 1 (WalletInfo Wallet)))
    selectedWallet /\ setSelectedWallet <- useState' $ Nothing

    useEffectOnce do
      liftEffect (Wallet.cardano =<< window) >>= case _ of
        Nothing -> pure unit
        Just cardano -> launchAff_ do
          eternl <- liftEffect (Wallet.eternl cardano) >>= traverse walletInfo
          -- gerowallet <- liftEffect (Wallet.gerowallet cardano) >>= traverse walletInfo
          lace <- liftEffect (Wallet.lace cardano) >>= traverse walletInfo
          nami <- liftEffect (Wallet.nami cardano) >>= traverse walletInfo
          yoroi <- liftEffect (Wallet.yoroi cardano) >>= traverse walletInfo
          case ArrayAL.fromArray (Proxy :: Proxy 1) (Array.catMaybes [ lace, nami, yoroi ]) of
            Nothing -> liftEffect $ onWalletConnect NoWallets
            Just wallets -> liftEffect $ do
              setWallets (Just wallets)
              setSelectedWallet $ do
                { name } <- un WalletInfo <$> currentlyConnected
                Array.find (\(WalletInfo wallet) -> wallet.name == name) (ArrayAL.toArray wallets)
      pure (pure unit)

    let
      onSubmit = case selectedWallet of
        Just selected@(WalletInfo s) ->
          if Just s.name == (_.name <<< unwrap <$> currentlyConnected) then onDismiss
          else launchAff_ do
            possibleApi <- (Just <$> Wallet.enable s.wallet) `catchError` \error -> do
              liftEffect $ onWalletConnect (ConnectionError error)
              pure Nothing
            case possibleApi of
              Just (walletApi :: Wallet.Api) -> do
                let
                  selected' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) selected
                liftEffect $ onWalletConnect (Connected selected')
              Nothing -> do
                -- FIXME: Error handling
                liftEffect $ onDismiss
        Nothing -> onDismiss

    pure $ do
      let
        { formBody, formActions } = case possibleWallets of
          Nothing ->
            { formBody: spinner Nothing
            , formActions: mempty
            }
          Just wallets -> do
            let
              choices = wallets <#> \(WalletInfo { icon, name }) ->
                Form.choice name $ DOM.span {}
                  [ DOOM.img { src: icon, alt: name, className: "w-2rem me-2" }
                  , DOOM.span_ [ DOOM.text name ]
                  ]

            { formBody: singleChoiceField
                { initialValue: fromMaybe "" (_.name <<< unwrap <$> selectedWallet)
                , onValueChange: \walletName ->
                    setSelectedWallet $ Array.find (\(WalletInfo wallet) -> wallet.name == walletName) (ArrayAL.toArray wallets)
                , type: Form.RadioButtonField choices
                }
            , formActions: DOOM.fragment
                [ link { label: DOOM.text "Cancel", onClick: onDismiss, showBorders: true }
                , DOOM.button do
                    let
                      _name :: forall wallet. Maybe (WalletInfo wallet) -> Maybe String
                      _name = map $ (_.name <<< unwrap)
                      selectedIsConnected = _name selectedWallet == _name currentlyConnected

                    { type: "button"
                    , className: "btn btn-primary"
                    , onClick: handler_ onSubmit
                    , disabled: selectedIsConnected
                    , children: [ DOOM.text "Connect wallet" ]
                    }
                ]
            }
      if inModal then modal
        { onDismiss -- : setConfiguringWallet false
        , footer: formActions
        , body: formBody
        , title: DOOM.text "Connect wallet"
        }
      else
        DOM.div { className: "d-flex flex-column align-items-center" }
          [ DOM.div { className: "d-flex flex-column align-items-center" }
              [ DOOM.h2_ [ DOOM.text "Connect wallet" ]
              , formBody
              ]
          , formActions
          ]
