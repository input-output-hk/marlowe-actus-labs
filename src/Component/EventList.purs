module Component.EventList where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), _abs, evalVal')
import CardanoMultiplatformLib.Types (unsafeBech32)
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource)
import Component.Widgets (link)
import Data.Argonaut (decodeJson, fromObject)
import Data.Array (catMaybes, concat, fromFoldable, singleton)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration as Duration
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Now (nowDateTime)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), Party, Token(..), Value)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (defaultRiskFactors, evalVal, toMarloweCashflow)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web (post')
import Marlowe.Runtime.Web.Client (GetResourceResponse, getResource)
import Marlowe.Runtime.Web.Types (ContractEndpoint(..), ContractHeader(..), IndexEndpoint(..), Metadata(..), PostTransactionsRequest(..), PostTransactionsResponse(..), ResourceEndpoint(..), ResourceLink(..), Runtime(..), TransactionsEndpoint(..), partyToBech32)
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

      onApplyInputs { party, token, value, endpoint: ContractEndpoint (ResourceEndpoint link) } = handler_ do
        now <- nowDateTime
        launchAff_ $
          getResource runtime.serverURL link {}
            >>= case _ , partyToBech32 party of
              Right { payload: { links: { transactions } } }, Just changeAddress -> do
                let
                  inputs = singleton $ IDeposit party party token value

                  invalidBefore = now
                  invalidHereafter = fromMaybe now $ adjust (Duration.Minutes 2.0) now

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

                post' runtime.serverURL transactions req
                  >>= case _ of
                    Right ({ resource: PostTransactionsResponse res }) -> do
                      traceM res
                      pure unit
                    Left _ -> do
                      traceM "error"
                      pure unit

                pure unit
              Left err, _ -> do
                -- Note: this happens, when the contract is in status `Unsigned`
                traceM err
                pure unit
              _, _ -> pure unit

        updateState _ { newInput = Nothing }

    pure $
      DOM.div {}
        [ case state.newInput of
            Just input@{ token, value, party } -> modal $
              { body:
                  DOM.form {} $
                    [ DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Amount"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: show (BigInt.abs value)
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
                    , DOM.div { className: "form-group" }
                        [ DOM.label
                            { className: "form-control-label" }
                            "Party"
                        , R.input
                            { className: "form-control"
                            , type: "text"
                            , value: partyToString party
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
                      , onClick: onApplyInputs input { value = BigInt.abs value }
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
  pure $ catMaybes $
    map
      ( \cf@(CashFlow { currency, amount }) -> do
          value <- evalVal' amount
          if value == (BigInt.fromInt 0) then Nothing
          else pure $
            { cashflow: toMarloweCashflow cf
            , party: if value < (BigInt.fromInt 0) then party else counterParty
            , token: currencyToToken currency
            , value
            , endpoint: links.contract
            }
      )
      projectedCashFlows

-- FIXME: proper mapping
currencyToToken :: String -> Token
currencyToToken = Token ""

partyToString :: Party -> String
partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role

-- partyToAddress :: Party -> Bech32
-- partyToAddress (V1.Address addr) = RT.Address addr
-- partyToAddress (V1.Role _) = RT.Address "" -- FIXME

actusMetadata
  :: { links :: { contract :: ContractEndpoint }
     , resource :: ContractHeader
     }
  -> Maybe M.Metadata
actusMetadata { resource: ContractHeader { metadata } } = decodeMetadata metadata

decodeMetadata :: Metadata -> Maybe M.Metadata
decodeMetadata (Metadata md) = lookup actusMetadataKey md >>= hush <<< decodeJson
