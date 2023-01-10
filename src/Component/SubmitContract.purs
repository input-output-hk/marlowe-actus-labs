module Component.SubmitContract where

import Prelude
import Prelude

import Actus.Domain (CT(..), CashFlow, ContractTerms(..))
import Actus.Domain.ContractTerms (ContractTerms)
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction as Transaction
import Component.ContractForm (initialJson, mkContractForm)
import Component.ContractForm as ContractForm
import Component.EventList (decodeMetadata)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (ContractHeaderResource, MkComponentM, WalletInfo(..))
import Component.Widgets (link, linkWithIcon)
import Contrib.Fetch (FetchError(InvalidStatusCode))
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (encodeJson)
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query as Query
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import HexString as HexString
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata (Metadata(..), actusMetadataKey, encodeMetadataObject)
import Marlowe.Runtime.Web.Client (ClientError(..), foldMapMPages, foldMapMPages', getResource, post, post')
import Marlowe.Runtime.Web.Types (CborHex(..), ContractEndpoint(..), ContractHeader(..), ContractsEndpoint(..), Metadata, PostContractsRequest(..), PostContractsResponse(..), Runtime(..), ServerURL(..), TextEnvelope(..), TxOutRef, txOutRefToString)
import Marlowe.Runtime.Web.Types as RT
import Polyform.Validator (runValidator)
import React.Basic as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet

type Props =
  { contractData :: ContractForm.Result
  , inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  , connectedWallet :: WalletInfo Wallet.Api
  }

data SubmissionStep
  = Creating
  | Created (Either String PostContractsResponse)
  | Signing (Either String PostContractsResponse)
  | Signed (Either ClientError PostContractsResponse)

mkSubmitContract :: MkComponentM (Props -> JSX)
mkSubmitContract = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "SubmitContract" \{ contractData, connectedWallet, inModal, onDismiss, onSuccess } -> React.do
    step /\ setStep <- useState' Creating
    let
      onSubmit = handler preventDefault \_ -> do
        launchAff_ $ do
          submit contractData runtime.serverURL runtime.root >>= case _ of
            Right res@{ resource: PostContractsResponse postContractsResponse, links: { contract: contractEndpoint } } -> do
              traceM "Response"
              traceM res
              let
                { contractId, txBody } = postContractsResponse
                TextEnvelope { cborHex: CborHex cborHex } = txBody
                lib = Lib.props cardanoMultiplatformLib
              traceM cborHex
              case HexString.decode <$> HexString.hex cborHex of
                Just txBytes -> do
                  traceM "DECODED HEX BYTES OF THE TX BODY"
                  tx <- liftEffect $ Transaction.transaction.from_bytes lib."Transaction" txBytes

                  traceM "Successfully created a transaction"
                  let
                    WalletInfo { wallet: walletApi } = connectedWallet
                  witnesses <- Wallet.signTx walletApi (Wallet.Cbor cborHex) false `catchError` \err -> do
                    traceM "Error signing the transaction: "
                    traceM err
                    throwError err
                  traceM "SIGNED"
                  traceM witnesses
                  case witnesses of
                    Right w -> do
                      pure unit
                    Left e -> do
                      traceM "Error from the wallet signTx"
                      traceM e

                Nothing ->
                  traceM "Hex decoding failed"
            Left err ->
              traceM $ "Error: " <> show err
    pure $ do
      let
        body = table { striped: Table.striped.boolean true, responsive: Table.responsive.sm, size: Table.sm } do
          let
            ContractTerms ct = contractData.contractTerms
            row header value = DOOM.tr_ do
              [ DOM.th { scope: "row" } [ DOOM.text header ]
              , DOOM.td_
                  [ DOM.div { className: "text-truncate w-32rem" } [ DOOM.text value ] ]
              ]
          DOOM.tbody_ do
            [ row "Party (you)" (partyToString $ contractData.party)
            , row "Counterparty" (partyToString $ contractData.counterParty)
            , row "Contract type" (show ct.contractType)
            ]
        -- FIXME: Introduce nice rendering of a contract here
        -- <> case ct.contractType of
        --       PAM -> [ row "Notional" (show ct.notional)
        --              , row "Currency" (show ct.currency)
        --              , row "Interest rate" (show ct.interestRate)
        --              , row "Interest rate cycle" (show ct.interestRateCycle)
        --              -- , row "Day count convention" (show ct.dayCountConvention)
        --              , row "Maturity date" (show ct.maturityDate)
        --              , row "PAM schedule" (show ct.pamSchedule)
        --              ]
        --       _ -> mempty
        footer = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
              do
                { className: "btn btn-primary"
                , onClick: onSubmit
                }
              [ DOOM.text "Submit" ]
          ]

      if inModal then modal
        { title: DOOM.text "Submit contract"
        , onDismiss
        , body
        , footer
        , size: Modal.Large
        }
      else
        DOOM.fragment
          [ body
          , footer
          ]

submit :: ContractForm.Result -> ServerURL -> ContractsEndpoint -> Aff _
submit contractData serverUrl contractsEndpoint = do
  let
    { contractTerms, contract, party, counterParty, changeAddress } = contractData
    metadata = RT.Metadata $ Map.singleton actusMetadataKey $ encodeJson $ Metadata
      { contractTerms: contractTerms
      , party
      , counterParty
      }

    req = PostContractsRequest
      { metadata
      -- , version :: MarloweVersion
      -- , roles :: Maybe RolesConfig
      , contract
      , minUTxODeposit: V1.Lovelace (BigInt.fromInt 2_000_000)
      , changeAddress: changeAddress
      , addresses: [ changeAddress ]
      , collateralUTxOs: []
      }

  post' serverUrl contractsEndpoint req

-- FIXME: paluh. Submission testing.
--  >>= case _ of
--    Right res -> do
--      pure unit
--    Left (FetchError (InvalidStatusCode res)) -> do
--      traceM "STATUS CODE ERROR"
--      traceM $ res.status
--      traceM $ res.statusText
--      body <- res.text
--      traceM "BODY:"
--      traceM body
--    Left _ -> do
--      traceM "OTHER error"
--      pure unit

partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role
