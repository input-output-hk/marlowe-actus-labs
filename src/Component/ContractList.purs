module Component.ContractList where

import Prelude

import Actus.Domain (ContractTerms)
import Data.Argonaut (parseJson)
import Data.Array (filter)
import Data.Array.NonEmpty (cons')
import Data.Either (Either(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (throw)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader(..), TxOutRef(..), ResourceWithLinks, toResourceLink)
import React.Basic.DOM (css, text)
import React.Basic.DOM as R
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.DOM.Generated as GDOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Component, Hook, UseState, component, useState, (/\))
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import React.Basic.Hooks as React
import Test.QuickCheck (mkSeed)
import Test.QuickCheck.Gen (Gen, elements, runGen)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type SubmissionError = String

type ContractId = TxOutRef

type ContractTermsV1 = ContractTerms V1.Value

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated ContractTermsV1

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

mkContractForm = component "ContractForm" \onNewContract -> React.do
  (validationResult /\ updateValidationResult) <- useState NotValidated
  (value /\ onValueChange) <- useInput ""

  let
    renderValidation = case validationResult of
      NotValidated -> mempty
      Failure validationMessage ->
        DOM.div { style: css { color: "red" } } [ text validationMessage ]
      Validated contract -> text "SUCCESS"

    validateForm = case parseJson value of
      Right json ->
        updateValidationResult (const $ Failure "STUB: JSON validated we should proceed further with actus validation here")
      Left _ -> updateValidationResult (const $ Failure "Invalid json")
  pure
    $ R.form
        { onSubmit: handler preventDefault (const $ validateForm)
        , children:
            [ R.input
                { placeholder: "Contract"
                , value
                , onChange: onValueChange
                }
            , renderValidation
            , R.input
                { type: "submit" }
            ]
        }

data NewContractState
  = Creating
  | Submitting (ContractTermsV1)
  | SubmissionError ContractTermsV1 SubmissionError
  | SubmissionsSuccess ContractTermsV1 ContractId

type ContractListState =
  { contractList :: Array (ResourceWithLinks ContractHeader (contract :: ContractEndpoint))
  , newContract :: Maybe NewContractState
  }

mkContractList :: Effect (_ -> JSX)
mkContractList = do
  contractForm <- mkContractForm

  component "ContractList" \contractList -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { contractList, newContract: Nothing }
    let
      onAddContractClick = handler_ do
        updateState \state -> state { newContract = Just Creating }

      onNewContract contractTerms = updateState _ { newContract = Just (Submitting contractTerms) }

    pure $ case state of
      { newContract: Just Creating } -> DOM.div {}
        [ DOM.title {} [ text "Add Contract" ]
        , contractForm onNewContract
        ]
      { newContract: Just (Submitting _) } -> text "Submitting"
      { newContract: Just _ } -> text "STUB: Other contract creation step"
      { newContract: Nothing } -> DOM.div {} $
        [ DOM.a
            { onClick: onAddContractClick, href: "#" }
            "Add Contract"
        ]
          <> map
            ( \({ links }) ->
                DOM.div
                  { className: "contracts-list" }
                  [ text $ unwrap $ toResourceLink links.contract ]
            )
            state.contractList

-- <<< filter
--   ( \{ resource } ->
--       let
--         ContractHeader header = resource
--       in
--         header.metadata == empty -- TODO: find ACTUS contract in Metadata
--   )

