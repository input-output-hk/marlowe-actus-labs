module Component.EventList where

import Prelude

import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ContractForm (walletChangeAddress)
import Component.Modal (mkModal)
import Component.Types (ActusContractRole(..), CashFlowInfo(..), ContractInfo(..), MkComponentM, UserContractRole(..), WalletInfo(..))
import Component.Widgets (link)
import Component.Widgets.Form (mkBooleanField)
import Contrib.Fetch (FetchError)
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Contrib.React.Bootstrap.Types as Bootstrap
import Control.Monad.Reader.Class (asks)
import Data.Array (elem, singleton)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.Either (Either(..), hush)
import Data.Foldable (foldMap)
import Data.Formatter.DateTime (formatDateTime)
import Data.Lazy as Lazy
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Time.Duration as Duration
import Debug (traceM)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), Party, Token, Value(Constant, DivValue))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (currenciesWith6Decimals, evalVal)
import Marlowe.Runtime.Web (post')
import Marlowe.Runtime.Web.Client (put')
import Marlowe.Runtime.Web.Types (PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), Runtime(..), ServerURL, TextEnvelope(..), TransactionEndpoint, TransactionsEndpoint, toTextEnvelope)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (span_, text) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (walletAddresses)

data NewInput
  = Creating
  | Submitting Contract
  | SubmissionError
  | SubmissionsSuccess

type EventListState =
  { newInput ::
      Maybe
        { party :: V1.Party
        , token :: Token
        , value :: BigInt.BigInt
        , transactionsEndpoint :: TransactionsEndpoint
        }
  -- , cashFlows :: Array { cashflow :: CashFlow Value Party, party :: Party, token :: Token, value :: BigInt.BigInt, transactions :: TransactionsEndpoint }
  }

type Props =
  { contractList :: Array ContractInfo
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

testingApply :: Boolean
testingApply = false

submit :: CborHex TransactionWitnessSetObject -> ServerURL -> TransactionEndpoint -> Aff (Either FetchError Unit)
submit witnesses serverUrl transactionEndpoint = do
  let
    textEnvelope = toTextEnvelope witnesses ""

    req = PutTransactionRequest textEnvelope
  put' serverUrl transactionEndpoint req

mkEventList :: MkComponentM (Props -> JSX)
mkEventList = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  booleanField <- liftEffect mkBooleanField
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "EventList" \{ contractList, connectedWallet } -> React.do
    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing }

    let
      onEdit { party, token, value, transactionsEndpoint } = handler_ do
        updateState _ { newInput = Just { party, token, value, transactionsEndpoint } }

      onApplyInputs { party, token, value, transactionsEndpoint } cw = handler_ do
        now <- nowDateTime
        -- FIXME: move aff flow into `useAff` on the component level
        launchAff_ $ do
          walletChangeAddress cardanoMultiplatformLib cw
            >>= case _ of
              Just changeAddress -> do

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

                post' runtime.serverURL transactionsEndpoint req
                  >>= case _ of
                    Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionEndpoint } }) -> do
                      traceM postTransactionsResponse
                      let
                        { txBody: tx } = postTransactionsResponse
                        TextEnvelope { cborHex: txCborHex } = tx
                      Wallet.signTx cw txCborHex true >>= case _ of
                        Right witnessSet -> do
                          submit witnessSet runtime.serverURL transactionEndpoint >>= case _ of
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
                      traceM token
                      traceM $ BigInt.toString value
                      traceM "error"
                      pure unit

                pure unit
              Nothing -> do
                -- Note: this happens, when the contract is in status `Unsigned`
                pure unit

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
                            , value: BigInt.toString (BigInt.abs value)
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

        , DOM.div { className: "row justify-content-end" } do
            let
              disabled = true -- isNothing connectedWallet
              showMyContracts = booleanField
                { disabled
                , initialValue: true
                , label: DOOM.text "Show my cash flows"
                , onToggle: const $ pure unit
                }
            DOM.div { className: "col-3 text-end" } do
              if disabled then do
                let
                  tooltipJSX = tooltip {} (DOOM.text "Connect to a wallet to add a contract")
                overlayTrigger
                  { overlay: tooltipJSX
                  , placement: Bootstrap.placement.bottom
                  }
                  -- Disabled button doesn't trigger the hook,
                  -- so we wrap it in a `span`
                  (DOOM.span_ [ showMyContracts ])
              else
                showMyContracts

        , table { striped: Table.striped.boolean true, hover: true } $
            [ DOM.thead {} do
              let
                th label = DOM.th { className: "text-center" } [ text label ]
              [ DOM.tr {}
                  [ th "Contract Id"
                  , th "Type"
                  , th "Date"
                  , th "Amount"
                  , th "Currency"
                  , th "Action"
                  ]
              ]
            , DOM.tbody {} $
                map
                  ( \(ContractInfo contractInfo@{ endpoints, userContractRole }) ->
                      let
                        cashFlowInfo = Lazy.force contractInfo.cashFlowInfo
                        tdCentered = DOM.td { className: "text-center" }
                      in
                        cashFlowInfo `flip foldMap` \(CashFlowInfo { cashFlow, sender, token, value }) -> do
                          let
                            cf = unwrap cashFlow
                            party = case sender of
                              ActusParty -> contractInfo.party
                              ActusCounterParty -> contractInfo.counterParty
                          [ DOM.tr {}
                              [ DOM.td {} [ text $ cf.contractId ]
                              , tdCentered [ text $ show cf.event ]
                              , tdCentered [ foldMap text $ hush (formatDateTime "YYYY-DD-MM HH:mm:ss" cf.paymentDay) ]
                              , DOM.td {className: "text-end"}
                                [ text $ fromMaybe "" $
                                  if elem cf.currency currenciesWith6Decimals
                                  then show <$> (((_ / 1000000.0) <<< BigInt.toNumber) <$> evalVal cf.amount)
                                  else BigInt.toString <$> (evalVal $ DivValue cf.amount (Constant $ BigInt.fromInt 1000000))
                                  ]
                              , tdCentered [ text $ if cf.currency == "" then "₳" else cf.currency ]
                              , tdCentered $ Array.singleton $ case endpoints.transactions of
                                  Just transactionsEndpoint -> do
                                    let
                                      button = DOM.button
                                        { onClick: onEdit { party, token, value, transactionsEndpoint }, className: "btn btn-secondary btn-sm" }
                                        "Execute"
                                    case sender, userContractRole of
                                      _, Just BothParties -> button
                                      ActusParty, Just ContractParty -> button
                                      ActusCounterParty, Just ContractCounterParty -> button
                                      _, _ -> mempty
                                  Nothing -> mempty
                              ]
                          ]
                  )
                  contractList
            ]
        ]

partyToString :: Party -> String
partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role

