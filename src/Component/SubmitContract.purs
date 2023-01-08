module Component.SubmitContract where

import Prelude
import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import Component.ContractForm (initialJson, mkContractForm)
import Component.ContractForm as ContractForm
import Component.EventList (decodeMetadata)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (ContractHeaderResource, WalletInfo, MkComponentM)
import Component.Widgets (link, linkWithIcon)
import Contrib.Fetch (FetchError(InvalidStatusCode))
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Table (table)
import Contrib.React.Bootstrap.Table as Table
import Contrib.React.Bootstrap.Types as OverlayTrigger
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
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus.Metadata (Metadata(..), actusMetadataKey, encodeMetadataObject)
import Marlowe.Runtime.Web.Client (ClientError(..), foldMapMPages, foldMapMPages', getResource, post, post')
import Marlowe.Runtime.Web.Types (ContractEndpoint(..), ContractHeader(..), Metadata, PostContractsRequest(..), PostContractsResponse(..), Runtime(..), TxOutRef, txOutRefToString)
import Marlowe.Runtime.Web.Types as RT
import Polyform.Validator (runValidator)
import React.Basic as DOOM
import React.Basic.DOM (text)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet

type Props =
  { contractData :: ContractForm.Result
  , inModal :: Boolean
  , onDismiss :: Effect Unit
  , onSuccess :: ContractEndpoint -> Effect Unit
  }

mkSubmitContract :: MkComponentM (Props -> JSX)
mkSubmitContract = do
  Runtime runtime <- asks _.runtime
  modal <- liftEffect mkModal

  liftEffect $ component "SubmitContract" \{ contractData, inModal, onDismiss, onSuccess } -> React.do
    let
      onSubmit = handler preventDefault \_ -> do
        launchAff_ $ submit contractData runtime.serverURL runtime.root

    pure $ do
      let
        body = table { striped: Table.striped.boolean true, responsive: Table.responsive.sm, size: Table.sm } do
          let
            row header value = DOOM.tr_ do
              [ DOM.th { scope: "row" } [ DOOM.text header ]
              , DOOM.td_
                  [ DOM.div { className: "text-truncate w-32rem" } [ DOOM.text value ] ]
              ]
          DOOM.tbody_
            [ row "Party (you)" (partyToString $ contractData.party)
            , row "Counterparty" (partyToString $ contractData.counterParty)
            ]
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

submit contractData serverUrl rootEndpoint = do
  let
    { contractTerms, contract, party, counterParty, changeAddress } = contractData
    metadata = RT.Metadata $ Map.singleton actusMetadataKey $ encodeMetadataObject $ Metadata
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

  post' serverUrl rootEndpoint req >>= case _ of
    Right ({ resource: PostContractsResponse res }) -> do
      traceM res
      pure unit
    Left (FetchError (InvalidStatusCode res)) -> do
      traceM "STATUS CODE ERROR"
      traceM $ res.status
      traceM $ res.statusText
      body <- res.text
      traceM "BODY:"
      traceM body
    Left _ -> do
      traceM "OTHER error"
      pure unit
  pure mempty

partyToString (V1.Address addr) = addr
partyToString (V1.Role role) = role
