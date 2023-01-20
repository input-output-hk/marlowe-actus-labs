module Component.ContractList where

import Prelude

import Actus.Domain (CashFlow)
import Actus.Domain.ContractTerms (ContractTerms)
import Component.ConnectWallet (walletInfo)
import Component.ContractForm (initialJson, mkContractForm, mkForm)
import Component.ContractForm as ContractForm
import Component.EventList (decodeMetadata)
import Component.Modal (mkModal)
import Component.SubmitContract (mkSubmitContract)
import Component.Types (ContractHeaderResource, MkComponentM, WalletInfo(..))
import Component.Widgets (linkWithIcon)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap (overlayTrigger, tooltip)
import Contrib.React.Bootstrap.Icons as Icons
import Contrib.React.Bootstrap.Types as OverlayTrigger
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Class (asks)
import Data.Array as Array
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.FormURLEncoded.Query as Query
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Newtype as Newtype
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (error, throw)
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web.Types (ContractHeader(..), Metadata, TxOutRef, txOutRefToString)
import Polyform.Validator (runValidator)
import React.Basic.DOM (text)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (Hook, JSX, UseState, component, useEffectOnce, useState, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Type.Prelude (Proxy(..))
import Wallet as Wallet
import Web.HTML (window)

type ContractId = TxOutRef

type ProjectedCashFlows = List (CashFlow Decimal Party)

type ValidationError = String

data FormState
  = NotValidated
  | Failure ValidationError
  | Validated (ContractTerms /\ Contract)

-- An example of a simple "custom hook"
useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

type SubmissionError = String

data NewContractState
  = Creating
  | Submitting ContractForm.Result
  | SubmissionError SubmissionError
  | SubmissionsSuccess ContractTerms ContractId

type ContractListState =
  { newContract :: Maybe NewContractState
  , metadata :: Maybe Metadata
  }

type Props =
  { contractList :: Array ContractHeaderResource
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

testingSubmit :: Boolean
testingSubmit = false

mkContractList :: MkComponentM (Props -> JSX)
mkContractList = do
  contractForm <- mkContractForm
  submitContract <- mkSubmitContract
  modal <- liftEffect $ mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib
  logger <- asks _.logger

  liftEffect $ component "ContractList" \{ connectedWallet, contractList } -> React.do
    ((state :: ContractListState) /\ updateState) <- useState { newContract: Nothing, metadata: Nothing }

    -- FIXME: paluh. Submission testing.
    internalConnectedWallet /\ setInternalConnectedWallet <- useState' Nothing

    useEffectOnce $ do
      when testingSubmit do
        let
          -- nami-work
          myAddress = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

          -- nami-test
          counterPartyAddress = "addr_test1qrp6m3r307r3d73t6vjnqssqmj9deqcprkm5v0yhuyvyfgm6fftzwd90f7aanfwl28s4efxxt3252p3uet87klt2aj4qzgw242"

          UseForm.Form { validator } = mkForm cardanoMultiplatformLib

          query = Query.fromHomogeneous
            { "party": [ myAddress ]
            , "counter-party": [ counterPartyAddress ]
            , "contract-terms": [ initialJson ]
            }

        launchAff_ do
          possibleNami <- liftEffect (window >>= Wallet.cardano) >>= case _ of
            Nothing -> pure Nothing
            Just cardano -> do
              liftEffect (Wallet.nami cardano) >>= traverse walletInfo >>= case _ of
                Nothing -> pure Nothing
                Just walletInfo@(WalletInfo { wallet }) -> do
                  walletApi <- Wallet.enable_ wallet
                  pure $ Just $ Newtype.over WalletInfo (Record.set (Proxy :: Proxy "wallet") walletApi) walletInfo
          liftEffect $ setInternalConnectedWallet possibleNami

        runValidator validator query >>= case _ of
          V (Right result) -> do
            updateState _ { newContract = Just $ Submitting result }
          V (Left err) -> do
            logger $ unsafeStringify err
            pure unit
      pure (pure unit)

    let
      onAddContractClick = updateState _ { newContract = Just Creating }

      onNewContract contractTerms = do
        updateState _ { newContract = Just (Submitting contractTerms) }

      onView metadata = handler_ do
        updateState _ { metadata = Just metadata }

    pure $
      DOOM.div_
        [ case state.newContract, connectedWallet <|> internalConnectedWallet of
            Just Creating, Just cw -> contractForm
              { onDismiss: updateState _ { newContract = Nothing }
              , onSuccess: onNewContract
              , inModal: true
              , connectedWallet: cw
              }
            Just (Submitting contractData), Just wallet -> submitContract
              { onDismiss: updateState _ { newContract = Nothing }
              , onSuccess: const $ pure unit
              , connectedWallet: wallet
              , inModal: true
              , contractData
              }
            Just _, _ ->
              modal
                { title: text "Success or failure"
                , onDismiss: updateState _ { newContract = Nothing }
                , body:
                    text ("Success or failure...")
                }
            Nothing, _ -> mempty
        , DOM.div { className: "row justify-content-end" } $ Array.singleton $ do
            let
              disabled = isNothing connectedWallet
              addContractLink = linkWithIcon
                { icon: Icons.fileEarmarkPlus
                , label: DOOM.text "Add contract"
                , disabled
                , onClick: onAddContractClick
                }
            DOM.div { className: "col-3 text-end" } $ Array.singleton $
              if disabled then do
                let
                  tooltipJSX = tooltip {} (DOOM.text "Connect to a wallet to add a contract")
                overlayTrigger
                  { overlay: tooltipJSX
                  , placement: OverlayTrigger.placement.bottom
                  }
                  -- Disabled button doesn't trigger the hook,
                  -- so we wrap it in a `span`
                  (DOOM.span_ [ addContractLink ])
              else
                addContractLink
        , DOM.div { className: "row" } $ Array.singleton $ case state.metadata of
            Just (metadata) -> modal $
              { body: text $ maybe "Empty Metadata" (show <<< _.contractTerms <<< unwrap) $ decodeMetadata metadata -- TODO: encode contractTerms as JSON
              , onDismiss: updateState _ { metadata = Nothing }
              , title: text "Contract Terms"
              }
            Nothing -> mempty
        , DOM.div { className: "row" } $ Array.singleton $
            DOM.table { className: "table table-striped table-hover" }
              [ DOM.thead {} $
                  [ DOM.tr {}
                      [ DOM.th {} [ text "Id" ]
                      , DOM.th {} [ text "Type" ]
                      , DOM.th {} [ text "Party" ]
                      , DOM.th {} [ text "Counter Party" ]
                      , DOM.th {} [ text "Terms" ]
                      , DOM.th {} [ text "Status" ]
                      ]
                  ]
              , DOM.tbody {} $ map
                  ( \{ resource: ContractHeader { contractId, status, metadata } } ->
                      let
                        md = decodeMetadata metadata
                      in
                        DOM.tr {}
                          [ DOM.td {} [ text $ maybe "" (_.contractId <<< unwrap <<< _.contractTerms <<< unwrap) md ]
                          , DOM.td {} [ text $ maybe "" (show <<< _.contractType <<< unwrap <<< _.contractTerms <<< unwrap) md ]
                          , DOM.td {} [ foldMap (displayParty <<< _.party <<< unwrap) md ]
                          , DOM.td {} [ foldMap (displayParty <<< _.counterParty <<< unwrap) md ]
                          , DOM.td {} [ DOM.button { onClick: onView metadata, className: "btn btn-secondary btn-sm" } "View" ]
                          , DOM.td {} $ do
                              let
                                tooltipJSX = tooltip {} (DOOM.text $ txOutRefToString contractId)
                              overlayTrigger
                                { overlay: tooltipJSX
                                , placement: OverlayTrigger.placement.bottom
                                } $ DOM.span {} [ show status ]
                          ]
                  )
                  contractList
              ]
        ]
  where
  displayParty :: Party -> JSX
  displayParty = case _ of
    V1.Role role -> render role (DOOM.text role)
    V1.Address addr -> render addr (DOM.div { className: "text-truncate w-16rem" } [ DOOM.text addr ])
    where
    render msg body =
      overlayTrigger
        { overlay: tooltip {} (DOOM.text $ msg)
        , placement: OverlayTrigger.placement.bottom
        }
        $ DOM.span {} [ body ]
