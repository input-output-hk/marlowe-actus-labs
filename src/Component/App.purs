module Component.App where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractTerms) as Actus
import Actus.Domain (CashFlow(..), Value'(..))
import Component.ConnectWallet (mkConnectWallet, walletInfo)
import Component.ConnectWallet as ConnectWallet
import Component.ContractList (mkContractList)
import Component.EventList (mkEventList)
import Component.MessageHub (mkMessageBox, mkMessagePreview)
import Component.Modal (Size(..), mkModal)
import Component.Types (ActusContractRole(..), CashFlowInfo(..), ContractInfo(..), MessageContent(Success, Info), MessageHub(MessageHub), MkComponentMBase, UserCashFlowDirection(..), UserContractRole(..), WalletInfo(..))
import Component.Types.ContractInfo (MarloweInfo(..))
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
import Contrib.React.Svg (SvgUrl(..), svgImg)
import Control.Monad.Error.Class (catchError)
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
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Now (now)
import Halogen.Subscription (Emitter) as Subscription
import Language.Marlowe.Core.V1.Semantics (emptyState, evalValue)
import Language.Marlowe.Core.V1.Semantics.Types (Environment(..), TimeInterval(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (currencyToToken, defaultRiskFactors)
import Marlowe.Actus (toMarloweValue, toMarloweCashflow) as Actus
import Marlowe.Actus.Metadata (Metadata(..)) as Actus
import Marlowe.Actus.Metadata as Actus.Metadata
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsEvent, ContractWithTransactionsMap, ContractWithTransactionsStream(..))
import Marlowe.Runtime.Web.Types (BlockHeader(..), SlotNumber(..), partyToBech32)
import Marlowe.Runtime.Web.Types as Runtime
import Marlowe.Time (unsafeInstantFromInt)
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
  = Lace
  | Yoroi
  | Nami
  | Eternl

instance Show WalletBrand where
  show Yoroi = "Yoroi"
  show Nami = "Nami"
  show Lace = "Lace"
  show Eternl = "Eternl"

autoConnectWallet :: WalletBrand -> (WalletInfo Wallet.Api -> Effect Unit) -> Aff Unit
autoConnectWallet walletBrand onSuccess = liftEffect (window >>= Wallet.cardano) >>= case _ of
  Nothing -> do
    -- We use this function in development mode, so we can just throw an error
    liftEffect $ throw $ "Missing \"cardano\" window attr"
  Just cardano -> do
    let
      extractWallet = case walletBrand of
        Lace -> Wallet.lace
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
debugWallet = Just Nami -- Nothing

data DisplayOption = Default | About

derive instance Eq DisplayOption

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
        Nothing, Just _ -> do
          liftEffect $ setWalletContext Nothing
        Just (WalletInfo walletInfo), _ -> do
          let
            action = do
              walletContext <- WalletContext.walletContext cardanoMultiplatformLib walletInfo.wallet
              liftEffect $ setWalletContext $ Just walletContext
          action `catchError` \_ -> do
            -- FIXME: Report back (to the reporting backend) a wallet problem?
            traceM "ERROR during wallet context construction"
            pure unit

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

    pure $ provider walletInfoCtx ((/\) <$> possibleWalletInfo <*> possibleWalletContext) $ Array.singleton $ DOM.div { className: "mt-6" } $
      [ DOM.div { className: "fixed-top" }
          [ DOM.nav { className: "navbar mb-lg-3 navbar-expand-sm navbar-light bg-light py-0" } $
              DOM.div { className: "container-xl" }
                [ DOM.a { href: "#", className: "navbar-brand" }
                    [ svgImg { src: marloweLogoUrl, height: "30px", className: "me-2" }
                    , DOOM.text "Marlowe Actus Labs"
                    ]
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
                    ]
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
      , Monoid.guard (displayOption == About)
          $ modal
          $
            { onDismiss: setDisplayOption Default
            , title: DOOM.text "About"
            , body: DOOM.div { dangerouslySetInnerHTML: { __html: about } }
            , size: Large
            }
      , Monoid.guard configuringWallet do
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
          jsx
      , DOM.div { className: "container-xl" } do
          let
            renderTab props children = tab props $ DOM.div { className: "row pt-4" } children
            contractArray = Array.fromFoldable contracts
          [ tabs { fill: true, justify: true, defaultActiveKey: "contracts" }
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
      if walletChanged then case partyToBech32 party, partyToBech32 counterParty of
        Just addr1, Just addr2 -> case Array.elem addr1 usedAddresses, Array.elem addr2 usedAddresses of
          true, true -> Just BothParties
          true, false -> Just ContractParty
          false, true -> Just ContractCounterParty
          false, false -> Nothing
        _, _ -> Nothing
      else prevRole

    map = Map.catMaybes $ updates <#> \{ contract: { resource: contractHeader@(Runtime.ContractHeader { contractId, block }), links: endpoints }, contractState, transactions } -> do
      let
        marloweInfo = do
          Runtime.ContractState contractState' <- contractState
          pure $ MarloweInfo
            { initialContract: contractState'.initialContract
            , state: contractState'.state
            , currentContract: contractState'.currentContract
            }

      case contractId `Map.lookup` prev of
        Just (ContractInfo contractInfo) -> do
          let
            userContractRole = mkUserContractRole
              contractInfo.userContractRole
              contractInfo.party
              contractInfo.counterParty

            cashFlowInfo =
              Lazy.defer \_ -> contractCashFlowInfo
                block
                contractInfo.contractTerms
                contractInfo.party
                contractInfo.counterParty
                contractInfo.marloweInfo
                userContractRole
                transactions

          pure $ ContractInfo $ contractInfo
            { cashFlowInfo = cashFlowInfo
            , userContractRole = userContractRole
            , marloweInfo = marloweInfo
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
                block
                contractTerms
                party
                counterParty
                marloweInfo
                userContractRole
                transactions
            , contractId
            , contractTerms
            , counterParty
            , endpoints
            , marloweInfo
            , party
            , userContractRole
            , _runtime: { contractHeader, transactions }
            }
  AppContractInfoMap { walletContext, map }

contractCashFlowInfo
  :: Maybe BlockHeader
  -> Actus.ContractTerms
  -> V1.Party
  -> V1.Party
  -> Maybe MarloweInfo
  -> Maybe UserContractRole
  -> Array Runtime.TxHeader
  -> Array CashFlowInfo
contractCashFlowInfo (Just (BlockHeader { slotNo: SlotNumber slot })) contractTerms party counterParty (Just (MarloweInfo { state })) possibleUserContractRole transactions = do
  let
    cashFlows = genProjectedCashflows
      (party /\ counterParty)
      (defaultRiskFactors contractTerms)
      contractTerms
    cashFlows' = addChoiceMarker $ map Just cashFlows
    transactions' = map Just transactions <> (Array.replicate (length cashFlows') Nothing)
    transactionsAndCashFlows = Array.zipWith { tx: _, cf: _ } transactions' (Array.fromFoldable cashFlows')

  Array.catMaybes $
    map
      ( case _ of
          { cf: Just cashFlow@(Actus.CashFlow { currency, amount }), tx } -> do
            let
              -- FIXME: correct conversions
              slotZeroTime = 1655683200
              beginInterval = unsafeInstantFromInt (slot + slotZeroTime)
              endInterval = unsafeInstantFromInt (slot + slotZeroTime)
              environment = Environment { timeInterval: TimeInterval beginInterval endInterval }
              actusValue = evalValue environment (fromMaybe emptyState state) (Actus.toMarloweValue amount)

            value <- PositiveBigInt.fromBigInt $ BigInt.abs actusValue
            let
              sender =
                if actusValue < (BigInt.fromInt 0) then ActusParty
                else ActusCounterParty
            pure $ CashFlowInfo
              { cashFlow: Actus.toMarloweCashflow cashFlow
              , sender
              , token: currencyToToken currency
              , transaction: tx
              , userCashFlowDirection: possibleUserContractRole <#> case _, sender of
                  BothParties, _ -> InternalFlow /\ value
                  ContractParty, ActusParty -> OutgoingFlow /\ value
                  ContractCounterParty, ActusCounterParty -> OutgoingFlow /\ value
                  _, _ -> IncomingFlow /\ value
              , value: actusValue
              }
          _ -> Nothing
      )
      transactionsAndCashFlows
  where
  addChoiceMarker List.Nil = List.Nil
  addChoiceMarker (List.Cons (Just x) xs) | hasRiskFactor x = List.Cons Nothing (List.Cons (Just x) (addChoiceMarker xs))
  addChoiceMarker (List.Cons x xs) = List.Cons x (addChoiceMarker xs)

  hasRiskFactor (CashFlow { amount }) = hasRiskFactor' amount
    where
    hasRiskFactor' :: Value' -> Boolean
    hasRiskFactor' (UseValue' _) = true
    hasRiskFactor' (Constant' _) = false
    hasRiskFactor' (AddValue' a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (SubValue' a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (MulValue' a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (DivValue' a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (NegValue' a) = hasRiskFactor' a
    hasRiskFactor' (Cond' _ a b) = hasRiskFactor' a || hasRiskFactor' b

contractCashFlowInfo _ _ _ _ _ _ _ = mempty

marloweLogoUrl :: SvgUrl
marloweLogoUrl = SvgUrl "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB3aWR0aD0iNTAiIGhlaWdodD0iNjAiIHZpZXdCb3g9IjAgMCA1MCA2MCI+CiAgICA8ZGVmcz4KICAgICAgICA8cGF0aCBpZD0idG82NzR1ODZhYSIgZD0iTTAgMEw1MCAwIDUwIDYwIDAgNjB6Ii8+CiAgICA8L2RlZnM+CiAgICA8ZyBmaWxsPSJub25lIiBmaWxsLXJ1bGU9ImV2ZW5vZGQiPgogICAgICAgIDxnPgogICAgICAgICAgICA8ZyB0cmFuc2Zvcm09InRyYW5zbGF0ZSgtODQ5IC0zNjgpIHRyYW5zbGF0ZSg4NDkgMzY4KSI+CiAgICAgICAgICAgICAgICA8bWFzayBpZD0ibDk5N3EzbWw4YiIgZmlsbD0iI2ZmZiI+CiAgICAgICAgICAgICAgICAgICAgPHVzZSB4bGluazpocmVmPSIjdG82NzR1ODZhYSIvPgogICAgICAgICAgICAgICAgPC9tYXNrPgogICAgICAgICAgICAgICAgPHBhdGggZmlsbD0iIzAwRTM5QyIgZD0iTTUuOTEgNjBjLS41NiAwLTEuMTE3LS4wOC0xLjY1OS0uMjM5LTEuNTEzLS40NDMtMi43NjUtMS40NS0zLjUyMi0yLjgzNi0uNzU4LTEuMzg1LS45MzItMi45ODMtLjQ5LTQuNUwxMS4yIDE0Ljg0NmMuMzYtMS4yMzMgMS4wOTItMi4yOTQgMi4xMTctMy4wNjcgMS4wMjQtLjc3MiAyLjI0NC0xLjE4NCAzLjUyNy0xLjE4OWguMDI0YzEuMjc4IDAgMi40OTYuNDA0IDMuNTIyIDEuMTY3IDEuMDI2Ljc2NCAxLjc2MyAxLjgxNiAyLjEzMiAzLjA0M2wyLjM5NyA3Ljk3NyA1LjQxLTE4LjUyM2MuMzYtMS4yMzMgMS4wOTItMi4yOTQgMi4xMTgtMy4wNjZDMzMuNDcyLjQxNiAzNC42OTIuMDA1IDM1Ljk3NCAwaC4wMjJjMS4yNzkgMCAyLjQ5Ny40MDQgMy41MjMgMS4xNjggMS4wMjYuNzY1IDEuNzYzIDEuODE3IDIuMTMyIDMuMDQ1TDQ5Ljc0NyAzMS4yYy40NTQgMS41MTMuMjkzIDMuMTEzLS40NTQgNC41MDQtLjc0NyAxLjM5Mi0xLjk5IDIuNDA4LTMuNTAxIDIuODYzLS41NTQuMTY3LTEuMTI3LjI1Mi0xLjcwMS4yNTItMS4yNDMgMC0yLjQ4My0uNDA3LTMuNDktMS4xNDYtMS4wNDMtLjc2NS0xLjc5LTEuODI2LTIuMTYzLTMuMDY4bC0yLjM2Ni03Ljg4Ny01LjQwNSAxOC41MDZjLS4zNiAxLjIzNC0xLjA5MiAyLjI5NC0yLjExNyAzLjA2Ni0xLjAyNS43NzItMi4yNDQgMS4xODMtMy41MjcgMS4xODhIMjVjLTEuMjc4IDAtMi40OTYtLjQwMy0zLjUyMi0xLjE2Ny0xLjAyNi0uNzY0LTEuNzYzLTEuODE2LTIuMTMyLTMuMDQybC0yLjM5My03Ljk2NS01LjM3OCAxOC40MzhjLS4zNjUgMS4yNTItMS4xMTEgMi4zMjMtMi4xNTcgMy4wOTdDOC40MDcgNTkuNTg4IDcuMTYxIDYwIDUuOTEgNjAiIG1hc2s9InVybCgjbDk5N3EzbWw4YikiLz4KICAgICAgICAgICAgPC9nPgogICAgICAgIDwvZz4KICAgIDwvZz4KPC9zdmc+Cg=="
