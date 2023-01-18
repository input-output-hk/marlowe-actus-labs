module Component.EventList where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), evalVal')
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject(..))
import Component.ConnectWallet (mkConnectWallet)
import Component.ContractForm (walletAddresses, walletChangeAddress)
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource, MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson)
import Data.Array (catMaybes, concat, fromFoldable, singleton)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.Either (Either(..), hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration as Duration
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), Party, Token(..), Value)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (defaultRiskFactors, evalVal, toMarloweCashflow)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web (post')
import Marlowe.Runtime.Web.Client (getResource, put')
import Marlowe.Runtime.Web.Types (ContractEndpoint(..), ContractHeader(..), Metadata(..), PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), ResourceEndpoint(..), Runtime(..), ServerURL(..), TextEnvelope(..), TransactionEndpoint(..), partyToBech32, toTextEnvelope)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Wallet (Wallet)
import Wallet as Wallet

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

type Props =
  { contractList :: Array ContractHeaderResource
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

testingApply :: Boolean
testingApply = false


submit :: CborHex TransactionWitnessSetObject -> ServerURL -> TransactionEndpoint -> Aff _
submit witnesses serverUrl transactionEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""

    req = PutTransactionRequest textEnvelope
  put' serverUrl transactionEndpoint req


mkEventList :: MkComponentM (Props -> JSX)
mkEventList = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "EventList" \{ contractList, connectedWallet } -> React.do
    let
      actusContracts = concat $ map endpointAndMetadata contractList

    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }

    let
      onEdit { party, token, value, endpoint } = handler_ do
        updateState _ { newInput = Just { party, token, value, endpoint } }

      onApplyInputs { party, token, value, endpoint: ContractEndpoint (ResourceEndpoint link) } cw = handler_ do
        now <- nowDateTime
        -- FIXME: move aff flow into `useAff` on the component level
        launchAff_ $ do
          possibleChangeAddress <- walletChangeAddress cardanoMultiplatformLib cw
          getResource runtime.serverURL link {}
            >>= case _, possibleChangeAddress of
              Right { payload: { links: { transactions } } }, Just changeAddress -> do

                addresses <- walletAddresses cardanoMultiplatformLib cw

                let
                  inputs = singleton $ IDeposit party party token value

                  invalidBefore = fromMaybe now $ adjust (Duration.Minutes (-2.0)) now
                  invalidHereafter = fromMaybe now $ adjust (Duration.Minutes 2.0) now

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

                traceM "APPLYING INPUTS"
                post' runtime.serverURL transactions req
                  >>= case _ of
                    Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionsEndpoint }}) -> do
                      traceM postTransactionsResponse
                      let
                        { txBody: tx } = postTransactionsResponse
                        TextEnvelope { cborHex: txCborHex } = tx
                      Wallet.signTx cw txCborHex true >>= case _ of
                        Right witnessSet -> do
                          submit witnessSet runtime.serverURL transactionsEndpoint >>= case _ of
                            Right _ -> do
                              traceM "Successfully submitted the transaction"
                              -- liftEffect $ onSuccess contractEndpoint
                            Left err -> do
                              traceM "Error while submitting the transaction"
                              traceM err


                        Left err -> do
                          traceM err
                          pure unit

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
        [ case state.newInput, connectedWallet of
            Just input@{ token, value, party }, Just (WalletInfo { wallet: cw }) -> modal $
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
                      , onClick: onApplyInputs input { value = BigInt.abs value } cw
                      }
                      [ R.text "Submit" ]
                  ]
              }
            _, _ -> mempty
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
