module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32, bech32FromBytes, bech32FromString, bech32ToString, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (FormBuilder, BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array (elem, fromFoldable)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (fromInt, toNumber, toString)
import Data.Either (Either(..), hush)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (toUpper)
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics (emptyState, evalValue)
import Language.Marlowe.Core.V1.Semantics.Types (Environment(..), Party, TimeInterval(..), Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows, defaultRiskFactors, genContract, toMarloweCashflow)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
import Marlowe.Time (unixEpoch)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (walletAddresses)

type FormSpec m = UseForm.Form m Unit -- JSX

type Result =
  { contractTerms :: ContractTerms
  , cashFlows :: CashFlows
  , contract :: V1.Contract
  , counterParty :: V1.Party
  , party :: V1.Party

  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  }

type Props =
  { onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  , connectedWallet :: WalletInfo Wallet.Api
  }

createCashFlows :: Party -> Party -> ContractTerms -> CashFlows
createCashFlows party counterParty terms =
  genProjectedCashflows
    (party /\ counterParty)
    (defaultRiskFactors terms)
    terms

walletChangeAddress :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Maybe Bech32)
walletChangeAddress lib wallet = do
  Wallet.getChangeAddress wallet >>= case _ of
    Right address ->
      map Just $ liftEffect $ runGarbageCollector lib $ bech32FromBytes (cborHexToCbor address) NoProblem.undefined
    Left _ -> pure Nothing

initialJson :: String
initialJson = String.joinWith "\n"
  [ "{"
  , """ "contractType": "PAM", """
  , """ "contractID": "pam01", """
  , """ "statusDate": "2023-12-31T00:00:00", """
  , """ "contractDealDate": "2023-12-28T00:00:00", """
  , """ "currency": "DjedTestUSD", """
  , """ "notionalPrincipal": "1000", """
  , """ "initialExchangeDate": "2024-01-01T00:00:00", """
  , """ "maturityDate": "2025-01-01T00:00:00", """
  , """ "nominalInterestRate": "0.1", """
  , """ "cycleAnchorDateOfInterestPayment": "2025-01-01T00:00:00", """
  , """ "cycleOfInterestPayment": "P1YL0", """
  , """ "dayCountConvention": "30E360", """
  , """ "endOfMonthConvention": "SD", """
  , """ "premiumDiscountAtIED": "   0", """
  , """ "rateMultiplier": "1.0", """
  , """ "contractRole": "RPA" """
  , "}"
  ]

initialAddress :: String
initialAddress = "" -- "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

error :: forall errs. String -> Batteries.Errors' (raw :: Array String | errs)
error = Array.singleton <<< rawError

addressInput :: CardanoMultiplatformLib.Lib -> String -> String -> Maybe String -> FormBuilder Effect Bech32
addressInput cardanoMultiplatformLib label initial name = do
  let
    props =
      { initial
      , label: Just $ DOOM.text label
      , name
      , validator: Validator.liftFnMMaybe (const $ pure [ "Invalid address" ]) \str -> do
          bech32FromString cardanoMultiplatformLib str
      }
  FormBuilder.textInput props

mkForm :: CardanoMultiplatformLib.Lib -> BootstrapForm Effect Query Result
mkForm cardanoMultiplatformLib = FormBuilder.evalBuilder ado
  contractTerms <- FormBuilder.textArea
    { missingError: "Please provide contract terms JSON value"
    , initial: initialJson
    , validator: Validator.liftFnEither \jsonString -> do
        json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
        lmap (Array.singleton <<< show) (decodeJson json)
    , rows: 15
    , name: (Just $ "contract-terms")
    }
  partyAddress <- addressInput cardanoMultiplatformLib "Your address" "" $ Just "party"
  counterPartyAddress <- addressInput cardanoMultiplatformLib "Counter-party address" initialAddress $ Just "counter-party"
  let
    counterParty = bech32ToParty counterPartyAddress
    party = bech32ToParty partyAddress
    cashFlows = createCashFlows party counterParty contractTerms
    contract = genContract contractTerms cashFlows
  in
    { contractTerms
    , cashFlows
    , contract
    , counterParty
    , party

    -- Ugly hack
    , usedAddresses: []
    , changeAddress: partyAddress
    , collateralUTxOs: []
    }

mkContractForm :: MkComponentM (Props -> JSX)
mkContractForm = do
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  let
    form = mkForm cardanoMultiplatformLib

  liftEffect $ component "ContractForm" \{ connectedWallet, onSuccess, onDismiss, inModal } -> React.do
    changeAddresses /\ setChangeAddresses <- useState' Nothing

    let
      onSubmit = _.result >>> case _ of
        Just (V (Right result)) -> do

          case changeAddresses of
            Nothing -> do
              traceM "No change addresses"
              pure unit
            Just addresses -> do
              onSuccess $ result { usedAddresses = addresses }
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

    { formState, onSubmit: onSubmit', result } <- useForm { spec: form, onSubmit, validationDebounce: Seconds 0.5 }

    useEffectOnce $ do
      case connectedWallet of
        WalletInfo { wallet } -> launchAff_ do
          addrs <- walletAddresses cardanoMultiplatformLib wallet
          liftEffect $ setChangeAddresses $ Just addrs
          walletChangeAddress cardanoMultiplatformLib wallet >>= case _, Map.lookup (FieldId "party") formState.fields of
            Just changeAddress, Just { onChange } -> do
              liftEffect $ onChange [ bech32ToString changeAddress ]
            _, _ -> pure unit
      pure (pure unit)
    pure $ do
      let
        formBody = DOM.div { className: "form-group" } do
          let
            mb3 = DOM.div { className: "mb-3" }
            fields = UseForm.renderForm form formState
          DOOM.fragment
            (fields <#> \field -> mb3 field)
            <> case result of
              Just (V (Right { cashFlows })) ->
                DOM.table { className: "table table-hover" } $
                  [ DOM.thead {} $
                      [ DOM.tr {}
                          [ DOM.th {} [ DOOM.text "Contract Id" ]
                          , DOM.th {} [ DOOM.text "Type" ]
                          , DOM.th {} [ DOOM.text "Date" ]
                          , DOM.th {} [ DOOM.text "Amount" ]
                          , DOM.th {} [ DOOM.text "Currency" ]
                          ]
                      ]
                  , DOM.tbody {} $ fromFoldable $
                      map
                        ( \cashflow ->
                            let
                              { contractId, event, paymentDay, amount, currency } = unwrap cashflow
                            in
                              [ DOM.tr {}
                                  [ DOM.td {} [ DOOM.text contractId ]
                                  , DOM.td {} [ DOOM.text $ show event ]
                                  , DOM.td {} [ DOOM.text <$> hush (formatDateTime "YYYY-DD-MM HH:mm:ss:SSS" paymentDay) ]
                                  , DOM.td {}
                                      [ DOOM.text $ do
                                          -- empty state and environment at contract creation
                                          let
                                            environment = Environment { timeInterval: TimeInterval unixEpoch unixEpoch }
                                            state = emptyState
                                          show <<< (_ / 1000000.0) <<< toNumber $ evalValue environment state amount
                                      ]
                                  , DOM.td {} [ DOOM.text $ if elem (toUpper currency) [ "", "ADA" ] then "₳" else currency ]
                                  ]
                              ]
                        )
                        (map toMarloweCashflow cashFlows)
                  ]
              _ -> mempty
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

-- useInput :: String -> Hook (UseState String) (String /\ EventHandler)
-- useInput initialValue = React.do
--   value /\ setValue <- useState initialValue
--   let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
--   pure (value /\ onChange)
-- 

