module Component.ContractList where

import Prelude

import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
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

mkContractForm :: Effect (((ContractTerms Decimal) /\ Contract -> Effect Unit) -> JSX)
mkContractForm = component "ContractForm" \onNewContract -> React.do
  (validationResult /\ updateValidationResult) <- useState NotValidated
  (value /\ onValueChange) <- useInput ""

  let
    renderValidation = case validationResult of
      NotValidated -> mempty
      Failure validationMessage ->
        DOM.div { style: css { color: "red" } } [ text validationMessage ]
      Validated (terms /\ contract) -> text $ "SUCCESS" <> show contract

    validateForm = case parseJson value >>= decodeJson of
      Right terms -> do
        let
          termsMarlowe = toMarlowe terms

          role1 = Role "R1" -- FIXME
          role2 = Role "R2" -- FIXME

          riskFactors :: EventType -> DateTime -> RiskFactorsMarlowe
          riskFactors _ _ = RiskFactors -- FIXME
            { o_rf_CURS: fromInt 1
            , o_rf_RRMO: fromInt 1
            , o_rf_SCMO: fromInt 1
            , pp_payoff: fromInt 0
            }
            where
            fromInt = Constant' <<< BigInt.fromInt <<< (marloweFixedPoint * _)

          contract = genContract (role1 /\ role2) riskFactors termsMarlowe
        onNewContract (terms /\ contract)

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
  | Submitting (ActusTerms /\ Contract)
  | SubmissionError ActusTerms SubmissionError
  | SubmissionsSuccess ActusTerms ContractId

type ContractListState =
  { contractList :: Array (ResourceWithLinks ContractHeader (contract :: ContractEndpoint))
  , newContract :: Maybe NewContractState
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

  component "ContractList" \contractList -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { contractList, newContract: Nothing }
    let
      onAddContractClick = handler_ do
        updateState _ { newContract = Just Creating }

      onNewContract contractTerms = do
        updateState _ { newContract = Just (Submitting contractTerms) }

    case state of
      { newContract: Just Creating } -> pure $ DOM.div {}
        [ DOM.title {} [ text "Add Contract" ]
        , contractForm onNewContract
        ]
      { newContract: Just (Submitting contract) } -> pure $ text ("Submitting" <> show contract)
      { newContract: Just _ } -> pure $ text "STUB: Other contract creation step"
      { newContract: Nothing } -> pure $
        DOM.div {}
          [ DOM.a
              { onClick: onAddContractClick, href: "#" }
              "Add Contract"
          ] <> contractsTable state.contractList

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
