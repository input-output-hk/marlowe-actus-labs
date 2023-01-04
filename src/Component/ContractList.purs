module Component.ContractList where

import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import Component.ContractForm (mkContractForm)
import Component.EventList (decodeMetadata)
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource, WalletInfo, MkComponentM)
import Component.Widgets (linkWithIcon)
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Data.Array as Array
import Data.Decimal (Decimal)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party(..))
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata, TxOutRef, txOutRefToString)
import React.Basic.DOM (text)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet

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
  | Submitting (ContractTerms /\ Contract)
  | SubmissionError SubmissionError
  | SubmissionsSuccess ContractTerms ContractId

type ContractListState =
  { newContract :: Maybe NewContractState
  , metadata :: Maybe Metadata
  }

type Props =
  { contractList :: Array ContractHeaderResource
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  contractForm <- mkContractForm
  modal <- liftEffect $ mkModal

  liftEffect $ component "ContractList" \{ connectedWallet, contractList } -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { newContract: Nothing, metadata: Nothing }
    let
      onAddContractClick = updateState _ { newContract = Just Creating }

      onNewContract contractTerms = do
        updateState _ { newContract = Just (Submitting contractTerms) }

      onView metadata = handler_ do
        updateState _ { metadata = Just metadata }

    pure $
      DOOM.div_
        [ case state.newContract of
            Just Creating -> contractForm
              { onDismiss: updateState _ { newContract = Nothing }
              , onError: \error -> updateState _ { newContract = Just (SubmissionError error) }
              , onSuccess: onNewContract
              , inModal: true
              , connectedWallet
              }
            Just (Submitting contract) ->
              modal
                { title: text "Submitting"
                -- FIXME: Should we ignore dismisses - we are not able to cancel submission I can imagine?
                , onDismiss: updateState _ { newContract = Nothing }
                , body:
                    -- FIXME: We should still present the form
                    text ("Submitting" <> show contract)
                }
            -- FIXME: Just a stub...
            Just _ ->
              modal
                { title: text "Success or failure"
                , onDismiss: updateState _ { newContract = Nothing }
                , body:
                    text ("Success or failure...")
                }
            Nothing -> mempty
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
              { body: text $ maybe "Empty Metadata" (show <<< _.contractTerms <<< unwrap) $ decodeMetadata metadata
              , onDismiss: updateState _ { metadata = Nothing }
              , title: text "ACTUS Contract Terms"
              }
            Nothing -> mempty
        , DOM.div { className: "row" } $ Array.singleton $
            DOM.table { className: "table table-striped table-hover" }
              [ DOM.thead {} $
                  [ DOM.tr {}
                      [ DOM.th {} [ text "Status" ]
                      , DOM.th {} [ text "Contract ID" ]
                      , DOM.th {} [ text "ACTUS Contract ID" ]
                      , DOM.th {} [ text "Party" ]
                      , DOM.th {} [ text "Counter Party" ]
                      , DOM.th {} [ text "Contract Terms" ]
                      ]
                  ]
              , DOM.tbody {} $ map
                  ( \{ resource: ContractHeader { contractId, status, metadata } } ->
                      let
                        md = decodeMetadata metadata
                      in
                        DOM.tr {}
                          [ DOM.td {} [ text $ show status ]
                          , DOM.td {} [ text $ txOutRefToString contractId ]
                          , DOM.td {} [ text $ maybe "" (_.contractId <<< unwrap <<< _.contractTerms <<< unwrap) md ]
                          , DOM.td {} [ text $ maybe "" (displayParty <<< _.party <<< unwrap) md ]
                          , DOM.td {} [ text $ maybe "" (displayParty <<< _.counterParty <<< unwrap) md ]
                          , DOM.td {} [ DOM.button { onClick: onView metadata, className: "btn btn-secondary btn-sm" } "View" ]
                          ]
                  )
                  contractList
              ]
        ]
  where
  displayParty :: Party -> String
  displayParty (Role role) = role
  displayParty (Address address) = address
