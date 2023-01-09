module Component.EventList where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow)
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource)
import Component.Widgets (link)
import Data.Argonaut (decodeJson, fromObject)
import Data.Array (concat, fromFoldable, singleton)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), Party, Token(..), Value)
import Marlowe.Actus (defaultRiskFactors, evalVal, toMarloweCashflow)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web (post')
import Marlowe.Runtime.Web.Types (ContractEndpoint(..), ContractHeader(..), IndexEndpoint(..), Metadata(..), PostTransactionsRequest(..), PostTransactionsResponse(..), ResourceEndpoint(..), ResourceLink(..), Runtime(..), TransactionsEndpoint(..))
import Marlowe.Runtime.Web.Types as RT
import Marlowe.Time (unixEpoch)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM (text) as DOOM
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
  { newInput ::
      Maybe
        { party :: Party
        , token :: Token
        , value :: BigInt.BigInt
        , endpoint :: ContractEndpoint
        }
  }

mkEventList :: Runtime -> Effect (Array ContractHeaderResource -> JSX)
mkEventList (Runtime runtime) = do
  modal <- mkModal
  component "EventList" \contractList -> React.do
    let
      actusContracts = concat $ map endpointAndMetadata contractList

    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }

    let
      onEdit { party, token, value, endpoint } = handler_ do
        updateState _ { newInput = Just { party, token, value, endpoint } }

      onApplyInputs { party, token, value, endpoint: ContractEndpoint (ResourceEndpoint (ResourceLink link)) } = handler_ do

        let
          -- FIXME: just a stub
          inputs = singleton $ IDeposit party party token value

          invalidBefore = toDateTime unixEpoch
          invalidHereafter = toDateTime unixEpoch
          changeAddress = RT.Address ""
          addresses = []
          collateralUTxOs = []

          metadata = mempty

          req = PostTransactionsRequest
            { inputs
            , invalidBefore
            , invalidHereafter
            , metadata
            , changeAddress
            , addresses
            , collateralUTxOs
            }

        launchAff_ $
          let
            ep = TransactionsEndpoint (IndexEndpoint (ResourceLink link))
          in
            post' runtime.serverURL ep req
              >>= case _ of
                Right ({ resource: PostTransactionsResponse res }) -> do
                  traceM res
                  pure unit
                Left _ -> do
                  traceM "error"
                  pure unit

        updateState _ { newInput = Nothing }

    pure $
      DOM.div {}
        [ case state.newInput of
            Just input@{ token, value } -> modal $
              { body:
                  DOM.form {} $
                    [ DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Amount"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: show value
                            }
                        ]
                    , DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Token"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: show token
                            }
                        ]
                    ]

              , onDismiss: updateState _ { newInput = Nothing }
              , title: text "Apply inputs"
              , footer: DOOM.fragment
                  [ link
                      { label: DOOM.text "Cancel"
                      , onClick: updateState _ { newInput = Nothing }
                      , showBorders: true
                      }
                  , DOM.button
                      { className: "btn btn-primary"
                      , onClick: onApplyInputs input
                      }
                      [ R.text "Submit" ]
                  ]
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
                ( \({ cashflow, party, token, value, endpoint }) ->
                    let
                      cf = unwrap cashflow
                    in
                      [ DOM.tr {}
                          [ DOM.td {} [ text cf.contractId ]
                          , DOM.td {} [ text $ show cf.event ]
                          , DOM.td {} [ text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" cf.paymentDay) ]
                          , DOM.td {} [ text $ fromMaybe "" $ BigInt.toString <$> evalVal cf.amount ]
                          , DOM.td {} [ text $ cf.currency ]
                          , DOM.td {} [ DOM.button { onClick: onEdit { party, token, value, endpoint }, className: "btn btn-secondary btn-sm" } "Add" ]
                          ]
                      ]
                )
                actusContracts
            ]
        ]

endpointAndMetadata
  :: { links :: { contract :: ContractEndpoint }, resource :: ContractHeader }
  -> Array { cashflow :: CashFlow Value Party, party :: Party, token :: Token, value :: BigInt.BigInt, endpoint :: ContractEndpoint }
endpointAndMetadata { resource: ContractHeader { metadata }, links } = fromMaybe [] $ do
  M.Metadata { contractTerms, party, counterParty } <- decodeMetadata metadata
  let
    projectedCashFlows = fromFoldable $ genProjectedCashflows (party /\ counterParty) (defaultRiskFactors contractTerms) contractTerms
  pure $
    map
      ( \cf ->
          { cashflow: toMarloweCashflow cf
          , party
          , token: Token "" "" -- currencyToToken cashflow.currency
          , value: BigInt.fromInt 0 -- FIXME
          , endpoint: links.contract
          }
      )
      projectedCashFlows

currencyToToken :: String -> Token
currencyToToken _ = Token "" "" -- FIXME

actusMetadata
  :: { links :: { contract :: ContractEndpoint }
     , resource :: ContractHeader
     }
  -> Maybe M.Metadata
actusMetadata { resource: ContractHeader { metadata } } = decodeMetadata metadata

decodeMetadata :: Metadata -> Maybe M.Metadata
decodeMetadata (Metadata md) = lookup actusMetadataKey md >>= hush <<< decodeJson <<< fromObject
