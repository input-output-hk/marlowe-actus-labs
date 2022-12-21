module Component.ContractList where

import Prelude

import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
import Component.ContractForm (mkContractForm)
import Component.Modal (mkModal)
import Data.Argonaut (decodeJson, parseJson)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party(..))
import Marlowe.Actus (RiskFactorsMarlowe, genContract, toMarlowe)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader(..), ResourceWithLinks, TxOutRef, txOutRefToString)
import React.Basic.DOM (css, text)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useState, (/\))
import React.Basic.Hooks as React

type SubmissionError = String

type ContractId = TxOutRef

type ActusTerms = ContractTerms Decimal -- V1.Value

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated (ActusTerms /\ Contract)

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

data NewContractState
  = Creating
  | Submitting (ActusTerms /\ Contract)
  | SubmissionError ActusTerms SubmissionError
  | SubmissionsSuccess ActusTerms ContractId

type ContractListState =
  { newContract :: Maybe NewContractState
  }

mkContractList
  :: Effect
       ( Array
           { links ::
               { contract :: ContractEndpoint
               }
           , resource :: ContractHeader
           }
         -> JSX
       )
mkContractList = do
  contractForm <- mkContractForm
  modal <- mkModal

  component "ContractList" \contractList -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { newContract: Just Creating }
    let
      onAddContractClick = handler_ do
        updateState _ { newContract = Just Creating }

      onNewContract contractTerms = do
        updateState _ { newContract = Just (Submitting contractTerms) }

    pure $
      DOM.div {}
        [ case state.newContract of
            Just Creating ->
              modal
                { title: text "Add contract"
                , onDismiss: updateState _ { newContract = Nothing }
                , body: contractForm onNewContract
                }
            -- [ DOM.title {} [ text "Add Contract" ]
            -- , contractForm onNewContract
            -- ]
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
        , DOM.a
            { onClick: onAddContractClick, href: "#" }
            "Add Contract"
        ] <> contractsTable contractList

contractsTable :: Array (ResourceWithLinks ContractHeader (contract :: ContractEndpoint)) -> JSX
contractsTable contractList =
  DOM.table { className: "table table-striped" } $
    [ DOM.thead {} $
        [ DOM.tr {}
            [ DOM.th {} [ text "Status" ]
            , DOM.th {} [ text "Contract ID" ]
            , DOM.th {} [ text "View" ]
            ]
        ]
    , DOM.tbody {} $ map contractRow contractList
    ]

contractRow :: ResourceWithLinks ContractHeader (contract :: ContractEndpoint) -> JSX
contractRow ({ resource: ContractHeader { contractId, status } }) =
  DOM.tr {}
    [ DOM.td {} [ text $ show status ]
    , DOM.td {} [ text $ txOutRefToString contractId ]
    , DOM.td {} [ DOM.a { href: "#", onClick: onEdit } "Edit" ]
    ]
  where
  onEdit = handler_ do
    pure unit

