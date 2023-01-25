module Component.App where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractTerms, evalVal') as Actus
import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Modal (Size(..), mkModal)
import Component.Types (ActusContractRole(..), CashFlowInfo(..), ContractInfo(..), MessageContent(Success, Info), MessageHub(MessageHub), MkComponentMBase, UserCashFlowDirection(..), UserContractRole(..), WalletInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.Data.BigInt.PositiveBigInt as PositiveBigInt
import Contrib.Data.Map (New(..), Old(..), additions, deletions) as Map
import Contrib.Halogen.Subscription (MinInterval(..))
import Contrib.Halogen.Subscription (bindEffect, foldMapThrottle) as Subscription
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Offcanvas (offcanvas)
import Contrib.React.Bootstrap.Offcanvas as Offcanvas
import Contrib.React.Bootstrap.Tab (tab)
import Contrib.React.Bootstrap.Tabs (tabs)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map (catMaybes, empty, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Newtype (un)
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
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (currencyToToken, defaultRiskFactors)
import Marlowe.Actus (toMarloweCashflow) as Actus
import Marlowe.Actus.Metadata (Metadata(..)) as Actus
import Marlowe.Actus.Metadata as Actus.Metadata
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsEvent, ContractWithTransactionsMap, ContractWithTransactionsStream(..))
import Marlowe.Runtime.Web.Types (partyToBech32)
import Marlowe.Runtime.Web.Types as Runtime
import React.Basic (JSX)
import React.Basic as ReactContext
import React.Basic.DOM (div, img, span_, text) as DOOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, provider, readRef, useEffect, useState')
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import Record as Record
import Type.Prelude (Proxy(..))
import Utils.React.Basic.Hooks (useEmitter', useLoopAff, useStateRef, useStateRef')
import Wallet as Wallet
import WalletContext (WalletContext(..))
import WalletContext as WalletContext
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

-- | Use this switch to autoconnect the wallet for testing.
debugWallet :: Maybe WalletBrand
debugWallet = Nothing -- Just Nami -- Nothing

data DisplayOption = Default | About

type ContractInfoMap = Map Runtime.ContractId ContractInfo

-- On the app level we keep the previous wallet context
-- so we can reuse pieces of contract info from the previous
-- state in a safe manner.
newtype AppContractInfoMap = AppContractInfoMap
  { walletContext :: Maybe WalletContext
  , map :: ContractInfoMap
  }

mkApp :: MkComponentMBase () (Unit -> JSX)
mkApp = do
  messageBox <- liftEffect $ mkMessageBox
  messagePreview <- liftEffect $ mkMessagePreview
  modal <- liftEffect $ mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  subcomponents <- do
    contractListComponent <- mkContractList
    eventListComponent <- mkEventList
    connectWallet <- mkConnectWallet
    pure { contractListComponent, eventListComponent, connectWallet, messageBox }

  (ContractWithTransactionsStream contractStream) <- asks _.contractStream

  throttledEmitter :: Subscription.Emitter (List ContractWithTransactionsEvent) <- liftEffect $
    Subscription.foldMapThrottle (List.singleton) (MinInterval $ Milliseconds 500.0) contractStream.emitter

  initialVersion <- liftEffect now

  walletInfoCtx <- asks _.walletInfoCtx
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  about <- asks _.aboutMarkdown

  liftEffect $ component "App" \_ -> React.do
    possibleWalletInfo /\ setWalletInfo <- useState' Nothing
    let
      walletInfoName = _.name <<< un WalletInfo <$> possibleWalletInfo
    possibleWalletInfoRef <- useStateRef walletInfoName possibleWalletInfo

    possibleWalletContext /\ setWalletContext <- useState' Nothing
    possibleWalletContextRef <- useStateRef' possibleWalletContext

    useLoopAff walletInfoName (Milliseconds 5000.0) do
      pwi <- liftEffect $ readRef possibleWalletInfoRef
      pwc <- liftEffect $ readRef possibleWalletContextRef
      case pwi, pwc of
        Nothing, Nothing -> pure unit
        Nothing , Just _ -> liftEffect $ setWalletContext Nothing
        Just (WalletInfo walletInfo), _ -> do
          walletContext <- WalletContext.walletContext cardanoMultiplatformLib walletInfo.wallet
          liftEffect $ setWalletContext $ Just walletContext

    configuringWallet /\ setConfiguringWallet <- useState' false
    checkingNotifications /\ setCheckingNotifications <- useState' false
    displayOption /\ setDisplayOption <- useState' Default

    -- We are ignoring contract events for now and we update the whole contractInfo set.
    upstreamVersion <- useEmitter' initialVersion (Subscription.bindEffect (const $ now) throttledEmitter)
    upstreamVersionRef <- useStateRef' upstreamVersion

    -- Let's use versioning so we avoid large comparison.
    (version /\ contractMap) /\ setContractMap <- useState' (upstreamVersion /\ AppContractInfoMap { walletContext: possibleWalletContext, map: Map.empty })
    idRef <- useStateRef version contractMap

    useEffect (upstreamVersion /\ possibleWalletContext) do
      updates <- contractStream.getLiveState
      old <- readRef idRef
      newVersion <- readRef upstreamVersionRef
      let
        new = updateAppContractInfoMap old possibleWalletContext updates
        _map (AppContractInfoMap { map }) = map

        old' = _map old
        new' = _map new

        (additionsNumber :: Int) = length $ Map.additions (Map.Old old') (Map.New new')
        (deletionsNumber :: Int) = length $ Map.deletions (Map.Old old') (Map.New new')

      when (deletionsNumber > 0 || additionsNumber > 0) do
        msgHubProps.add $ Info $ DOOM.text $
          "New contracts: " <> show additionsNumber <> ", deleted contracts: " <> show deletionsNumber

      setContractMap (newVersion /\ new)
      pure $ pure unit

    -- -- This causes a lot of re renders - we avoid it for now by
    -- -- enforcing manual offcanvas toggling.
    -- -- FIXME: expose the msgs emitter and use it to detect
    -- -- when message box is empty.
    -- msgs <- useContext msgHubProps.ctx
    -- useEffect (List.null msgs) do
    --   when (List.null msgs) do
    --     setCheckingNotifications false
    --   pure $ pure unit

    useAff unit $ for debugWallet \walletBrand ->
      autoConnectWallet walletBrand \walletInfo -> do
        liftEffect $ setWalletInfo $ Just walletInfo

    let
      AppContractInfoMap { map: contracts } = contractMap

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
                renderTab props children = tab props $ DOM.div { className: "row pt-4" } children
                contractArray = Array.fromFoldable contracts
              [ tabs {fill: true, justify: true, defaultActiveKey: "cash-flows" }
                [ renderTab
                  { eventKey: "contracts"
                  , title: DOM.div
                    { className: "text-body" }
                    [ DOM.span { className: "me-2" } $ Icons.toJSX Icons.files
                    , DOOM.text "Contracts"
                    ]
                  }
                  $ subcomponents.contractListComponent { contractList: contractArray, connectedWallet: possibleWalletInfo }
                , renderTab
                  { eventKey: "cash-flows"
                  , title: DOM.div
                    { className: "text-body" }
                    [ DOM.span { className: "me-2" } $ Icons.toJSX Icons.cashStack
                    , DOOM.text "Cash flows"
                    ]

                  }
                  $ subcomponents.eventListComponent { contractList: contractArray, connectedWallet: possibleWalletInfo }
                ]
              ]
          ]

-- TODO: Currently we ignore role tokens.
updateAppContractInfoMap :: AppContractInfoMap -> Maybe WalletContext -> ContractWithTransactionsMap -> AppContractInfoMap
updateAppContractInfoMap (AppContractInfoMap { walletContext: prevWalletContext, map: prev }) walletContext updates = do
  let
    walletChanged = prevWalletContext /= walletContext
    usedAddresses = fromMaybe [] $ _.usedAddresses <<< un WalletContext <$> walletContext

    mkUserContractRole prevRole party counterParty = do
      if walletChanged
      then case partyToBech32 party, partyToBech32 counterParty of
          Just addr1, Just addr2 -> case Array.elem addr1 usedAddresses, Array.elem addr2 usedAddresses of
            true, true -> Just BothParties
            true, false -> Just ContractParty
            false, true -> Just ContractCounterParty
            false, false -> Nothing
          _, _ -> Nothing
      else prevRole

    map = Map.catMaybes $ updates <#> \{ contract: { resource: contractHeader@(Runtime.ContractHeader { contractId }), links: endpoints }, transactions } -> do
      case contractId `Map.lookup` prev of
        Just (ContractInfo contractInfo) -> do
          let
            userContractRole = mkUserContractRole
              contractInfo.userContractRole
              contractInfo.party
              contractInfo.counterParty

            cashFlowInfo = do
              let
                recomputeCashFlows = walletChanged || transactions /= contractInfo._runtime.transactions
              if recomputeCashFlows
                then Lazy.defer \_ -> contractCashFlowInfo
                  contractInfo.contractTerms
                  contractInfo.party
                  contractInfo.counterParty
                  userContractRole
                  transactions
                else contractInfo.cashFlowInfo

          pure $ ContractInfo $ contractInfo
            { cashFlowInfo = cashFlowInfo
            , userContractRole = userContractRole
            , _runtime
              { contractHeader = contractHeader
              , transactions = transactions
              }
            }
        Nothing -> do
          Actus.Metadata { party, counterParty, contractTerms } <- Actus.Metadata.fromRuntimeResource contractHeader
          let
            Runtime.ContractHeader { contractId } = contractHeader
            userContractRole = mkUserContractRole Nothing party counterParty
          pure $ ContractInfo $
            { cashFlowInfo: Lazy.defer \_ -> contractCashFlowInfo
                contractTerms
                party
                counterParty
                userContractRole
                transactions
            , contractId
            , contractTerms
            , counterParty
            , endpoints
            , party
            , userContractRole
            , _runtime: { contractHeader, transactions }
            }
  AppContractInfoMap { walletContext, map }


contractCashFlowInfo
  :: Actus.ContractTerms
  -> V1.Party
  -> V1.Party
  -> Maybe UserContractRole
  -> Array Runtime.Tx
  -> Array CashFlowInfo
contractCashFlowInfo contractTerms party counterParty possibleUserContractRole transactions = do
  let
    numberOfTransactions = length transactions
    -- TODO: more reliable detection of active cashflows
    projectedCashFlows =
      Array.drop numberOfTransactions
      $ Array.fromFoldable
      $ genProjectedCashflows
        (party /\ counterParty)
        (defaultRiskFactors contractTerms)
        contractTerms

  Array.catMaybes $
    map
      ( \cf@(Actus.CashFlow { currency, amount }) -> do
          actusValue <- Actus.evalVal' amount
          value <- PositiveBigInt.fromBigInt $ BigInt.abs actusValue
          let
            sender = if actusValue < (BigInt.fromInt 0)
              then ActusParty
              else ActusCounterParty
          pure $ CashFlowInfo
            { cashFlow: Actus.toMarloweCashflow cf
            , sender
            , token: currencyToToken currency
            , userCashFlowDirection: possibleUserContractRole <#> case _, sender of
                BothParties, _ -> InternalFlow /\ value
                ContractParty, ActusParty -> OutgoingFlow /\ value
                ContractCounterParty, ActusCounterParty -> OutgoingFlow /\ value
                _, _ -> IncomingFlow /\ value
            , value: actusValue
            }
      )
      projectedCashFlows
