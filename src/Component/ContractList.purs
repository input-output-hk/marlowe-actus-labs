module Component.ContractList where

import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import Component.ConnectWallet (walletInfo)
import Component.ContractForm (initialJson, mkContractForm, mkForm)
import Component.ContractForm as ContractForm
import Component.Modal (mkModal)
import Component.SubmitContract (mkSubmitContract)
import Component.Types (ActusContractId(..), ContractInfo(..), MessageContent(..), MessageHub(..), MkComponentM, WalletInfo(..))
import Component.Types.ContractInfo as ContractInfo
import Component.Widget.Table as Table
import Component.Widgets (linkWithIcon)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Control.Alt ((<|>))
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.FormURLEncoded.Query as Query
import Data.Function (on)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (un, unwrap)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web.Streaming (ContractWithTransactions)
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata, TxOutRef, GetContractsResponse, txOutRefToString)
import Marlowe.Runtime.Web.Types as Runtime
import Polyform.Validator (runValidator)
import React.Basic as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet
import Web.HTML (window)

type ContractId = TxOutRef

type ProjectedCashFlows = List (CashFlow Decimal Party)

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated (ContractTerms /\ Contract)

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

type SubmissionError = String

data NewContractState
  = Creating
  | Submitting ContractForm.Result
  | SubmissionError SubmissionError
  | SubmissionsSuccess ContractTerms ContractId

type ContractListState =
  { newContract :: Maybe NewContractState
  , metadata :: Maybe Metadata
  }

type Props =
  { contractList :: Array ContractInfo
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

data OrderBy
  = OrderByCreationDate
  | OrderByActusContractId
  | OrderByLastUpdateDate

derive instance Eq OrderBy

testingSubmit :: Boolean
testingSubmit = false

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  contractForm <- mkContractForm
  submitContract <- mkSubmitContract
  modal <- liftEffect $ mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  logger <- asks _.logger
  msgHub@(MessageHub msgHubProps) <- asks _.msgHub

  liftEffect $ component "ContractList" \{ connectedWallet, contractList } -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { newContract: Nothing, metadata: Nothing }
    ordering /\ updateOrdering <- useState { orderBy: OrderByCreationDate, orderAsc: false }

    let
      contractList' = do
        let
          -- Quick and dirty hack to display just submited contracts as first
          someFutureBlockNumber = Runtime.BlockNumber 2058430
          sortedContracts = case ordering.orderBy of
            OrderByCreationDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.createdAt)) contractList
            OrderByActusContractId -> Array.sortBy (compare `on` (ContractInfo.actusContractId)) contractList
            OrderByLastUpdateDate -> Array.sortBy (compare `on` (fromMaybe someFutureBlockNumber <<< map (_.blockNo <<< un Runtime.BlockHeader) <<< ContractInfo.updatedAt)) contractList
        if ordering.orderAsc then sortedContracts
        else Array.reverse sortedContracts

    useEffectOnce $ do
      when testingSubmit do
        let
          -- nami-work
          myAddress = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

          -- nami-test
          counterPartyAddress = "addr_test1qrp6m3r307r3d73t6vjnqssqmj9deqcprkm5v0yhuyvyfgm6fftzwd90f7aanfwl28s4efxxt3252p3uet87klt2aj4qzgw242"

          UseForm.Form { validator } = mkForm cardanoMultiplatformLib

          query = Query.fromHomogeneous
            { "party": [ myAddress ]
            , "counter-party": [ counterPartyAddress ]
            , "contract-terms": [ initialJson ]
            }

        runValidator validator query >>= case _ of
          V (Right result) -> do
            updateState _ { newContract = Just $ Submitting result }
          V (Left err) -> do
            logger $ unsafeStringify err
            pure unit
      pure (pure unit)

    let
      onAddContractClick = updateState _ { newContract = Just Creating }

      onNewContract contractTerms = do
        updateState _ { newContract = Just (Submitting contractTerms) }

      onView metadata = do
        updateState _ { metadata = Just metadata }

    pure $
      DOOM.div_
        [ case state.newContract, connectedWallet of
            Just Creating, Just cw -> contractForm
              { onDismiss: updateState _ { newContract = Nothing }
              , onSuccess: onNewContract
              , inModal: true
              , connectedWallet: cw
              }
            Just (Submitting contractData), Just wallet -> submitContract
              { onDismiss: do
                  updateState _ { newContract = Nothing }
                  msgHubProps.add $ Error $ DOOM.text $ fold
                    [ "Contract submission failed. Please try again later."
                    ]
              , onSuccess: const $ do
                  updateState _ { newContract = Nothing }
                  msgHubProps.add $ Success $ DOOM.text $ fold
                    [ "Successfully submitted the contract. Contract transaction awaits to be included in the blockchain."
                    , "Contract status should change to 'Confirmed' at that point."
                    ]
              , connectedWallet: wallet
              , inModal: true
              , contractData
              }
            Just _, _ ->
              modal
                { title: text "Success or failure"
                , onDismiss: updateState _ { newContract = Nothing }
                , body:
                    text ("Success or failure...")
                }
            Nothing, _ -> mempty
        , DOM.div { className: "row justify-content-end" } $ Array.singleton $ do
            let
              disabled = isNothing connectedWallet
              addContractLink = linkWithIcon
                { icon: Icons.fileEarmarkPlus
                , label: DOOM.text "Add contract"
                , disabled
                , onClick: onAddContractClick
                }
            DOM.div { className: "col-3 text-end" } $ Array.singleton $
              if disabled then do
                let
                  tooltipJSX = tooltip {} (DOOM.text "Connect to a wallet to add a contract")
                overlayTrigger
                  { overlay: tooltipJSX
                  , placement: OverlayTrigger.placement.bottom
                  }
                  -- Disabled button doesn't trigger the hook,
                  -- so we wrap it in a `span`
                  (DOOM.span_ [ addContractLink ])
              else
                addContractLink
        , DOM.div { className: "row" } $ Array.singleton $ case state.metadata of
            Just (metadata) -> modal $
              { body: text $ maybe "Empty Metadata" (show <<< _.contractTerms <<< unwrap) $ M.decodeMetadata metadata -- TODO: encode contractTerms as JSON
              , onDismiss: updateState _ { metadata = Nothing }
              , title: text "Contract Terms"
              }
            Nothing -> mempty
        , table { striped: Table.striped.boolean true, hover: true }
            [ DOM.thead {} do
                let
                  orderingTh = Table.orderingHeader ordering updateOrdering
                  th label = DOM.th { className: "text-center text-muted" } [ label ]
                [ DOM.tr {}
                    [ orderingTh (DOOM.text "Actus Contract Id") OrderByActusContractId
                    , do
                        let
                          label = DOOM.fragment [ DOOM.text "Created" ] --, DOOM.br {},  DOOM.text "(Block number)"]
                        orderingTh label OrderByCreationDate
                    , th $ DOOM.text "Type"
                    , th $ DOOM.text "Party"
                    , th $ DOOM.text "Counter Party"
                    , th $ DOOM.text "Status"
                    , th $ DOOM.text "Contract"
                    ]
                ]
            , DOM.tbody {} $ map
                ( \ci@(ContractInfo { _runtime, counterParty, party }) -> -- { resource: ContractHeader { contractId, status, metadata } } ->
                    let
                      ContractHeader { contractId, status, metadata } = _runtime.contractHeader
                      -- md = M.decodeMetadata metadata
                      tdCentered = DOM.td { className: "text-center" }
                    in
                      DOM.tr {}
                        [ DOM.td {} [ text $ un ActusContractId $ ContractInfo.actusContractId ci ]
                        , tdCentered [ text $ foldMap show $ map (un Runtime.BlockNumber <<< _.blockNo <<< un Runtime.BlockHeader) $ ContractInfo.createdAt ci ]
                        , tdCentered [ text $ show <<< ContractInfo.actusContractType $ ci ]
                        , DOM.td {} [ displayParty party ]
                        , DOM.td {} [ displayParty counterParty ]
                        , DOM.td { className: "text-center" } $ do
                            let
                              tooltipJSX = tooltip {} (DOOM.text $ txOutRefToString contractId)
                            overlayTrigger
                              { overlay: tooltipJSX
                              , placement: OverlayTrigger.placement.bottom
                              } $ DOM.span {} [ show status ]
                        , tdCentered
                            [ linkWithIcon { icon: Icons.eye, label: DOOM.text "View", onClick: onView metadata } ]
                        ]
                )
                contractList'
            ]
        ]
  where
  displayParty :: Party -> JSX
  displayParty = case _ of
    V1.Role role -> render role (DOOM.text role)
    V1.Address addr -> render addr (DOM.div { className: "text-truncate w-16rem" } [ DOOM.text addr ])
    where
    render msg body =
      overlayTrigger
        { overlay: tooltip {} (DOOM.text $ msg)
        , placement: OverlayTrigger.placement.bottom
        }
        $ DOM.span {} [ body ]
