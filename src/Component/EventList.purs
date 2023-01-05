module Component.EventList where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), ContractTerms, Value')
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource)
import Data.Argonaut (decodeJson, fromObject)
import Data.Array (catMaybes, concat, fromFoldable)
import Data.BigInt.Argonaut as BigInt
import Data.Either (hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party, Value)
import Marlowe.Actus (defaultRiskFactors, evalVal, toMarloweCashflow)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader(..), Metadata(..))
import React.Basic.DOM (text)
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React

data NewInput
  = Creating
  | Submitting Contract
  | SubmissionError
  | SubmissionsSuccess

type EventListState =
  { newInput :: Maybe (CashFlow Value Party)
  }

mkEventList :: Effect (Array ContractHeaderResource -> JSX)
mkEventList = do
  modal <- mkModal
  component "EventList" \contractList -> React.do
    let
      termsList = catMaybes $ map actusTermsAndParties contractList
      projectedCashFlows (terms /\ parties) = fromFoldable $ genProjectedCashflows parties (defaultRiskFactors terms) terms
      (cashFlows :: Array (CashFlow Value' Party)) = concat $ map projectedCashFlows termsList

    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }

    let
      onEdit cf = handler_ do
        updateState _ { newInput = Just cf }

    pure $
      DOM.div {}
        [ case state.newInput of
            Just (CashFlow { currency, amount }) -> modal $
              { body:
                  DOM.form {} $
                    [ DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Amount"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: fromMaybe "" $ BigInt.toString <$> evalVal amount
                            }
                        ]
                    , DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Token"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: currency
                            }
                        ]
                    ]

              , onDismiss: updateState _ { newInput = Nothing }
              , title: text "Apply inputs"
              }
            Nothing -> mempty
        , DOM.table { className: "table table-hover" } $
            [ DOM.thead {} $
                [ DOM.tr {}
                    [ DOM.th {} [ text "Contract Id" ]
                    , DOM.th {} [ text "Type" ]
                    , DOM.th {} [ text "Date" ]
                    , DOM.th {} [ text "Amount" ]
                    , DOM.th {} [ text "Currency" ]
                    , DOM.th {} [ text "Add" ]
                    ]
                ]
            , DOM.tbody {} $ map
                ( ( \cashflow@(CashFlow cf) ->
                      [ DOM.tr {}
                          [ DOM.td {} [ text cf.contractId ]
                          , DOM.td {} [ text $ show cf.event ]
                          , DOM.td {} [ text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" cf.paymentDay) ]
                          , DOM.td {} [ text $ fromMaybe "" $ BigInt.toString <$> evalVal cf.amount ]
                          , DOM.td {} [ text $ cf.currency ]
                          , DOM.td {} [ DOM.button { onClick: onEdit cashflow, className: "btn btn-secondary btn-sm" } "Add" ]
                          ]
                      ]
                  )
                    <<< toMarloweCashflow
                )
                cashFlows
            ]
        ]

actusMetadata
  :: { links :: { contract :: ContractEndpoint }
     , resource :: ContractHeader
     }
  -> Maybe M.Metadata
actusMetadata { resource: ContractHeader { metadata } } = decodeMetadata metadata

actusTermsAndParties
  :: { links :: { contract :: ContractEndpoint }
     , resource :: ContractHeader
     }
  -> Maybe (ContractTerms /\ (Party /\ Party))
actusTermsAndParties { resource: ContractHeader { metadata } } = do
  md <- unwrap <$> decodeMetadata metadata
  pure (_.contractTerms md /\ (_.party md /\ _.counterParty md))

decodeMetadata :: Metadata -> Maybe M.Metadata
decodeMetadata (Metadata md) = lookup actusMetadataKey md >>= hush <<< decodeJson <<< fromObject
