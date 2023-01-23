module Component.App where

import Prelude

import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Modal (Size(..), mkModal)
import Component.Types (MessageContent(Success, Info), MessageHub(MessageHub), MkComponentMBase, WalletInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.Data.Map as Map
import Contrib.Halogen.Subscription (MinInterval(..))
import Contrib.Halogen.Subscription (foldMapThrottle) as Subscription
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Offcanvas (offcanvas)
import Contrib.React.Bootstrap.Offcanvas as Offcanvas
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Monoid as Monoid
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (now)
import Halogen.Subscription (Emitter) as Subscription
import Marlowe.Runtime.Web.Streaming (ContractEvent, ContractStream(..))
import Marlowe.Runtime.Web.Types (GetContractsResponse, TxOutRef)
import React.Basic (JSX)
import React.Basic as ReactContext
import React.Basic.DOM as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useEmitter, useStateRef)
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
    -- We use this function in development mode, so we can just throw an error
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
        Wallet.enable wallet >>= case _ of
          Right walletApi -> do
            let
              walletInfo' = Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) walletInfo
            liftEffect $ onSuccess walletInfo'
          -- FIXME: paluh - handle error
          Left _ -> pure unit

data DisplayOption = Default | About

mkApp :: MkComponentMBase () (Unit -> JSX)
mkApp = do
  messageBox <- liftEffect $ mkMessageBox
  messagePreview <- liftEffect $ mkMessagePreview
  modal <- liftEffect $ mkModal
  subcomponents <- do
    contractListComponent <- mkContractList
    eventListComponent <- mkEventList
    connectWallet <- mkConnectWallet
    pure { contractListComponent, eventListComponent, connectWallet, messageBox }

  (ContractStream contractStream) <- asks _.contractStream

  throttledEmitter :: Subscription.Emitter (List ContractEvent) <- liftEffect $
    Subscription.foldMapThrottle (List.singleton) (MinInterval $ Milliseconds 500.0) contractStream.emitter

  initialVersion <- liftEffect now

  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  about <- asks _.aboutMarkdown

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    configuringWallet /\ setConfiguringWallet <- useState' false
    checkingNotifications /\ setCheckingNotifications <- useState' false
    displayOption /\ setDisplayOption <- useState' Default
    (version /\ contracts) /\ setContracts <- useState' (initialVersion /\ Map.empty)

    idRef <- useStateRef version contracts

    useEmitter throttledEmitter \contractEvent -> do
      (currContracts :: Map TxOutRef GetContractsResponse) <- contractStream.getLiveState
      version <- now
      let
        (additionsNumber :: Int) = length $ Map.additions (Map.Old contracts) (Map.New currContracts)
        (deletionsNumber :: Int) = length $ Map.deletions (Map.Old contracts) (Map.New currContracts)

      msgHubProps.add $ Info $ DOOM.text $
        "New contracts: " <> show additionsNumber <> ", deleted contracts: " <> show deletionsNumber

      setContracts (version /\ currContracts)

    -- -- This causes a lot of re renders - we avoid it for now by
    -- -- enforcing manual offcanvas toggling.
    -- -- FIXME: expose the msgs emitter and use it to detect
    -- -- when message box is empty.
    -- msgs <- useContext msgHubProps.ctx
    -- useEffect (List.null msgs) do
    --   when (List.null msgs) do
    --     setCheckingNotifications false
    --   pure $ pure unit

    let
      debugWallet = Nothing
    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo

    pure $ provider walletInfoCtx possibleWalletInfo $ Array.singleton $ DOM.div { className: "mt-6" } $
      ( case displayOption of
          About -> Array.singleton $ modal $
            { onDismiss: setDisplayOption Default
            , title: DOOM.text "About"
            , body: DOOM.div { dangerouslySetInnerHTML: { __html: about } }
            , size: Large
            }
          Default -> []
      )
        <>
          [ DOM.div { className: "fixed-top" }
              [ DOM.nav { className: "navbar mb-lg-3 navbar-expand-sm navbar-light bg-light py-0" } $
                  DOM.div { className: "container-xl" }
                    [ DOM.a { href: "#", className: "navbar-brand" } "Marlowe Actus Labs"
                    , DOM.div { className: "navbar-collapse justify-content-end text-end" } $
                        [ DOM.ul { className: "navbar-nav gap-2" }
                            [ DOM.li { className: "nav-item" } $
                                linkWithIcon
                                  { icon: Icons.infoSquare
                                  , label: (DOOM.text "About")
                                  , extraClassNames: "nav-link"
                                  , onClick: setDisplayOption About
                                  }
                            , DOM.li { className: "nav-item" } $ ReactContext.consumer msgHubProps.ctx \msgs ->
                                [ linkWithIcon
                                    { icon: if List.null msgs then Icons.bellSlash else Icons.bellFill
                                    , label: DOOM.text "Notifications"
                                    , extraClassNames: "nav-link"
                                    , onClick: setCheckingNotifications true
                                    , disabled: List.null msgs
                                    }
                                ]
                            -- FIXME: This should be moved to submenu
                            -- , DOM.li { className: "nav-item" } $
                            --     linkWithIcon
                            --       { icon: Icons.cashStack
                            --       , label: DOOM.text "Cash flows"
                            --       , extraClassNames: "nav-link"
                            --       , onClick: pure unit
                            --       }
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
                            jsx = subcomponents.connectWallet
                              { currentlyConnected: possibleWalletInfo
                              , onWalletConnect: \result -> do
                                  case result of
                                    ConnectWallet.Connected walletInfo -> do
                                      let
                                        WalletInfo { name } = walletInfo
                                      msgHubProps.add $ Success $ DOOM.text $ "Connected to " <> name
                                      setWalletInfo (Just walletInfo)
                                    ConnectWallet.ConnectionError _ -> pure unit
                                    ConnectWallet.NoWallets -> pure unit
                                  setConfiguringWallet false
                              , onDismiss: setConfiguringWallet false
                              , inModal: true
                              }
                          [ jsx ]
                    ]
              , DOM.div { className: "container-xl" }
                  $ DOM.div { className: "row" }
                  $ messagePreview msgHub
              ]
          , ReactContext.consumer msgHubProps.ctx \_ ->
              pure $ offcanvas
                { onHide: setCheckingNotifications false
                , placement: Offcanvas.placement.end
                , show: checkingNotifications -- && (not $ List.null msgs)
                , scroll: false
                }
                [ DOM.div { className: "p-3 overflow-auto" } $ messageBox msgHub
                ]
          , DOM.div { className: "container-xl" } do
              let
                contracts' = Array.fromFoldable contracts
              [ subcomponents.contractListComponent { contractList: contracts', connectedWallet: possibleWalletInfo }
              , DOM.div { className: "col" } $ subcomponents.eventListComponent { contractList: contracts', connectedWallet: possibleWalletInfo }
              ]
          ]
