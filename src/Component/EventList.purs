module Component.EventList where

import Prelude

import Component.Modal (mkModal)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader)
import React.Basic.DOM (text)
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
  { newInput :: Maybe NewInput
  }

mkEventList
  :: Effect
       ( Array
           { links :: { contract :: ContractEndpoint }
           , resource :: ContractHeader
           }
         -> JSX
       )
mkEventList = do
  modal <- mkModal
  component "EventList" \eventList -> React.do
    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }

    let
      onEdit = handler_ do
        updateState _ { newInput = Just Creating }

    pure $
      DOM.div {}
        [ case state.newInput of
            Just _ -> modal $
              { body: text "body"
              , onDismiss: updateState _ { newInput = Nothing }
              , title: text "title"
              }
            Nothing -> mempty
        , DOM.table { className: "table table-striped" } $
            [ DOM.thead {} $
                [ DOM.tr {}
                    [ DOM.th {} [ text "Contract" ]
                    , DOM.th {} [ text "Type" ]
                    , DOM.th {} [ text "Date" ]
                    , DOM.th {} [ text "Amount" ]
                    , DOM.th {} [ text "Currency" ]
                    , DOM.th {} [ text "Contract ID" ]
                    ]
                ]
            -- , DOM.tbody {} $ map eventRow eventList
            ]
        ]
