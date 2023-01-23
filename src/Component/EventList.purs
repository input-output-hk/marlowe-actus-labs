module Component.EventList where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (CashFlow(..), evalVal')
import CardanoMultiplatformLib (CborHex)
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject)
import Component.ContractForm (walletAddresses, walletChangeAddress)
import Component.Modal (mkModal)
import Component.Types (ContractHeaderResource, MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson)
import Data.Array (catMaybes, concat, drop, filter, fromFoldable, length, singleton)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (adjust)
import Data.Decimal as D
import Data.Either (Either(..), either, hush)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Time.Duration as Duration
import Data.Traversable (traverse)
import Debug (traceM)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input(..), Party, Token(..), Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Extended.V1.Metadata (lovelaceFormat)
import Language.Marlowe.Extended.V1.Metadata.Types (NumberFormat(..))
import Marlowe.Actus (defaultRiskFactors, evalVal, toMarloweCashflow)
import Marlowe.Actus.Metadata (actusMetadataKey)
import Marlowe.Actus.Metadata as M
import Marlowe.Runtime.Web (getPage', post')
import Marlowe.Runtime.Web.Client (getResource, put')
import Marlowe.Runtime.Web.Types (ContractEndpoint(..), ContractHeader(..), IndexEndpoint(..), Metadata(..), PostTransactionsRequest(..), PostTransactionsResponse(..), PutTransactionRequest(..), ResourceEndpoint(..), Runtime(..), ServerURL(..), TextEnvelope(..), TransactionEndpoint(..), TransactionsEndpoint(..), api, toTextEnvelope)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
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
        , transactions :: TransactionsEndpoint
        }
  , cashFlows :: Array { cashflow :: CashFlow Value Party, party :: Party, token :: Token, value :: BigInt.BigInt, transactions :: TransactionsEndpoint }
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

-- liftEither :: forall a m err. MonadEffect m => Show err => Either err a -> m a
-- liftEither = either (liftEffect <<< throw <<< show) pure

mkEventList :: MkComponentM (Props -> JSX)
mkEventList = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "EventList" \{ contractList, connectedWallet } -> React.do
    ((state :: EventListState) /\ updateState) <- useState { newInput: Nothing, cashFlows: [] }

    let
      onEdit { party, token, value, transactions } = handler_ do
        updateState _ { newInput = Just { party, token, value, transactions } }

      onApplyInputs { party, token, value, transactions } cw = handler_ do
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

                traceM "APPLYING INPUTS"
                post' runtime.serverURL transactions req
                  >>= case _ of
                    Right ({ resource: PostTransactionsResponse postTransactionsResponse, links: { transaction: transactionsEndpoint } }) -> do
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
              Nothing -> do
                -- Note: this happens, when the contract is in status `Unsigned`
                pure unit

        updateState _ { newInput = Nothing }

    useAff unit $ do
      -- let
      --   isActus :: ContractHeaderResource -> Boolean
      --   isActus { resource: ContractHeader { metadata: Metadata md } } = isJust $ lookup actusMetadataKey md
      -- contracts <- filter isActus <$> (getPage' runtime.serverURL api Nothing >>= liftEither >>> liftEffect <#> _.page)
      -- cashFlows' <- traverse (cashFlowAndEndpoint runtime) contracts
      cashFlows' <- traverse (cashFlowAndEndpoint runtime) contractList
      liftEffect $ updateState _ { cashFlows = concat cashFlows' }

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
            , DOM.tbody {} $
                map
                  ( \({ cashflow, party, token, value, transactions }) ->
                      let
                        cf = unwrap cashflow
                      in
                        [ DOM.tr {}
                            [ DOM.td {} [ text cf.contractId ]
                            , DOM.td {} [ text $ show cf.event ]
                            , DOM.td {} [ text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" cf.paymentDay) ]
                            , DOM.td {}
                                [ text $ fromMaybe "" $
                                    if cf.currency == "" then show <$> (((_ / 1000000.0) <<< BigInt.toNumber) <$> evalVal cf.amount)
                                    else BigInt.toString <$> (evalVal $ DivValue cf.amount (Constant $ BigInt.fromInt 1000000))
                                ]
                            , DOM.td {} [ text $ if cf.currency == "" then "₳" else cf.currency ]
                            , DOM.td {} [ DOM.button { onClick: onEdit { party, token, value, transactions }, className: "btn btn-secondary btn-sm" } "Add" ]
                            ]
                        ]
                  )
                  state.cashFlows
            ]
        ]

cashFlowAndEndpoint
  :: forall r s t
   . { serverURL :: ServerURL | r }
  -> { links :: { contract :: ContractEndpoint | s }, resource :: ContractHeader | t }
  -> Aff
       ( Array
           { cashflow :: CashFlow Value Party
           , party :: Party
           , token :: Token
           , transactions :: TransactionsEndpoint
           , value :: BigInt.BigInt
           }
       )
cashFlowAndEndpoint { serverURL } { resource: ContractHeader { metadata }, links: { contract: ContractEndpoint (ResourceEndpoint link) } } =
  case decodeMetadata metadata of
    Just (M.Metadata { contractTerms, party, counterParty }) -> do
      getResource serverURL link {}
        >>= case _ of
          Right { payload: { links: { transactions: TransactionsEndpoint (IndexEndpoint link') } } } -> do
            numberOfTransactions <- getResource serverURL link' {}
              >>= case _ of
                Right { payload: arr } -> pure $ length arr
                _ -> pure 0

            let
              -- TODO: more reliable detection of active cashflows
              projectedCashFlows = drop numberOfTransactions $ fromFoldable $ genProjectedCashflows (party /\ counterParty) (defaultRiskFactors contractTerms) contractTerms

            pure $ catMaybes $
              map
                ( \cf@(CashFlow { currency, amount }) -> do
                    value <- evalVal' amount
                    if value == (BigInt.fromInt 0) then Nothing
                    else pure $
                      { cashflow: toMarloweCashflow cf
                      , party: if value < (BigInt.fromInt 0) then party else counterParty
                      , token: currencyToToken currency
                      , value: if currency == "" then value else value / (BigInt.fromInt 1000000)
                      , transactions: TransactionsEndpoint (IndexEndpoint link')
                      }
                )
                projectedCashFlows

          _ -> pure mempty -- FIXME: notification
    Nothing -> pure mempty

-- FIXME: proper mapping
currencyToToken :: String -> Token
currencyToToken = Token ""

tokenToCurrency :: Token -> String
tokenToCurrency (Token "" "") = "L"
tokenToCurrency _ = "undefined"

partyToString :: Party -> String
partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role

actusMetadata
  :: { links :: { contract :: ContractEndpoint }
     , resource :: ContractHeader
     }
  -> Maybe M.Metadata
actusMetadata { resource: ContractHeader { metadata } } = decodeMetadata metadata

decodeMetadata :: Metadata -> Maybe M.Metadata
decodeMetadata (Metadata md) = lookup actusMetadataKey md >>= hush <<< decodeJson
