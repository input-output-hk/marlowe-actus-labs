module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Address (Bech32(..), bech32FromHex, isValidBech32)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link, spinner)
import Component.Widgets.Form (FormSpec, useForm)
import Component.Widgets.Form (input) as Form
import Contrib.React.Bootstrap.Form (label) as Form
import Contrib.React.Bootstrap.Form.Control (textArea, textInput) as Form
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (defaultRiskFactors, genContract)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMEither) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (type (/\), Hook, UseState, component, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet

type Result = ContractTerms /\ V1.Contract

type Props =
  { onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

createContract :: Party -> Party -> ContractTerms -> V1.Contract
createContract party counterParty terms = do
  let
    cashflowsMarlowe = genProjectedCashflows
      (party /\ counterParty)
      (defaultRiskFactors terms)
      terms
  genContract cashflowsMarlowe

initialJson :: String
initialJson = String.joinWith "\n"
  [ "{"
  , """ "contractType": "PAM", """
  , """ "contractID": "pam01", """
  , """ "statusDate": "2012-12-30T00:00:00", """
  , """ "contractDealDate": "2012-12-28T00:00:00", """
  , """ "currency": "USD", """
  , """ "notionalPrincipal": "3000", """
  , """ "initialExchangeDate": "2013-01-01T00:00:00", """
  , """ "maturityDate": "2014-01-01T00:00:00", """
  , """ "nominalInterestRate": "0.1", """
  , """ "cycleAnchorDateOfInterestPayment": "2013-01-01T00:00:00", """
  , """ "cycleOfInterestPayment": "P1ML0", """
  , """ "dayCountConvention": "A365", """
  , """ "endOfMonthConvention": "SD", """
  , """ "premiumDiscountAtIED": "   0", """
  , """ "rateMultiplier": "1.0", """
  , """ "contractRole": "RPA" """
  , "}"
  ]

initialAddress :: String
initialAddress = "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

error :: forall errs. String -> Batteries.Errors' (raw :: Array String | errs)
error = Array.singleton <<< rawError

addressInput :: CardanoMultiplatformLib.Lib -> String -> String -> FormSpec Effect Query Party
addressInput cardanoMultiplatformLib name initial = Form.input (FieldId name) initial $ Validator.liftFnMEither \str -> do
  isValidBech32 cardanoMultiplatformLib str <#> if _
    then Right (V1.Address str)
    else Left $ error "Invalid address"

mkFormSpec :: CardanoMultiplatformLib.Lib -> FormSpec Effect Query Result
mkFormSpec cardanoMultiplatformLib = ado
  contractTerms <- Form.input (FieldId "contract-terms") initialJson $ Validator.liftFnEither \jsonString -> do
    json <- lmap (const $ error "Invalid JSON") $ parseJson jsonString
    lmap (error <<< show) (decodeJson json)
  party <- addressInput cardanoMultiplatformLib "party" ""
  counterParty <- addressInput cardanoMultiplatformLib "counter-party" initialAddress
  in
    contractTerms /\ createContract party counterParty contractTerms

mkContractForm :: MkComponentM (Props -> JSX)
mkContractForm = do
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  let
    formSpec = mkFormSpec cardanoMultiplatformLib

  liftEffect $ component "ContractForm" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    let
      onSubmit = _.result >>> case _ of
        Just (V( Right result)) -> onSuccess result
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    { fields, onSubmit: onSubmit', result } <- useForm { spec: formSpec, onSubmit, validationDebounce: Seconds 0.5 }

    useEffectOnce $ do
      case connectedWallet of
        Nothing -> pure unit
        Just (WalletInfo { wallet }) -> launchAff_ do
          Wallet.Address addressStr <- Wallet.getChangeAddress wallet
          liftEffect $ bech32FromHex cardanoMultiplatformLib addressStr >>= case _, Map.lookup (FieldId "party") fields of
            Just (Bech32 addr), Just { onChange } ->
              onChange [addr]
            _, _ -> do
               pure unit
      pure (pure unit)
    pure $ do
      let
        formBody = DOM.div { className: "form-group" } do
          let
            mb3 = DOM.div { className: "mb-3" }
          [ mb3
              [ Form.label { htmlFor: "json" } [ R.text "Contract JSON" ]
              , Form.textArea do
                  let
                    { value, errors, onChange } = fromMaybe mempty (Map.lookup (FieldId "contract-terms") fields)
                  { className: "form-control"
                  , id: "json"
                  , placeholder: "Please provide the contract JSON"
                  , value: fromMaybe "" $ Array.head value
                  , onChange: handler targetValue (onChange <<< Array.fromFoldable)
                  , isValid: maybe false null errors
                  , isInvalid: maybe false (not <<< null) errors
                  , rows: 15
                  }
              ]
          , mb3
              [ Form.label {} [ R.text "Your address" ]
              , do
                  let
                    { value, errors } = fromMaybe mempty (Map.lookup (FieldId "party") fields)
                  case value of
                    [] -> DOM.div { className: "text-truncate" } (spinner Nothing)
                    _ -> Form.textInput do
                      { id: "party"
                      , value: fromMaybe "" $ Array.head value
                      , isValid: maybe false null errors
                      , isInvalid: maybe false (not <<< null) errors
                      , disabled: true
                      }
              ]
          , mb3
              [ Form.label { htmlFor: "counter-party" } [ R.text "Counterparty address" ]
              , Form.textInput do
                  let
                    { value, errors, onChange, touched: Disj touched } = fromMaybe mempty (Map.lookup (FieldId "counter-party") fields)
                  { id: "counter-party"
                  , placeholder: "Enter counter party address"
                  , onChange: handler targetValue (onChange <<< Array.fromFoldable)
                  , value: fromMaybe "" $ Array.head value
                  , isValid: maybe false null errors
                  , isInvalid: touched && maybe false (not <<< null) errors
                  }
              ]
          ]
        formActions = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
            do
              let
                disabled = case result of
                  Just (V (Right _)) -> false
                  _ -> true
              { className: "btn btn-primary"
              , onClick: onSubmit'
              , disabled
              }
            [ R.text "Submit" ]
          ]

      if inModal then modal
        { title: R.text "Add contract"
        , onDismiss
        , body: formBody
        , footer: formActions
        , size: Modal.Large
        }
      else
        formBody

useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

