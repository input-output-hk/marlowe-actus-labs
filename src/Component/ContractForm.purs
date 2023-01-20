module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32, addressObject, allocate, asksLib, bech32FromBytes, bech32FromString, bech32ToString, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject)
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
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (defaultRiskFactors, genContract)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
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

type FormSpec m = UseForm.Form m Unit -- JSX

type Result =
  { contractTerms :: ContractTerms
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

createContract :: Party -> Party -> ContractTerms -> V1.Contract
createContract party counterParty terms = do
  let
    cashflowsMarlowe = genProjectedCashflows
      (party /\ counterParty)
      (defaultRiskFactors terms)
      terms
  genContract cashflowsMarlowe

walletChangeAddress :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Maybe Bech32)
walletChangeAddress lib wallet = do
  Wallet.getChangeAddress wallet >>= case _ of
    Right address ->
      map Just $ liftEffect $ runGarbageCollector lib $ bech32FromBytes (cborHexToCbor address) NoProblem.undefined
    Left err -> pure Nothing

walletAddresses :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Array Bech32)
walletAddresses cardanoMultiplatformLib wallet = do
  possibleUsedAddresses <- Wallet.getUsedAddresses_ wallet
  -- let
  --   possibleUsedAddresses = Right []
  possibleUTxOs <- Wallet.getUtxos_ wallet

  case possibleUsedAddresses, possibleUTxOs of
    Right addresses, Right (Just utxos) -> do
      liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
        utxoAddresses' <- for utxos \utxo -> do
          let
            utxo' = cborHexToCbor utxo
          unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput utxo'
          txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
          addressObj <- allocate $ transactionOutputObject.address txOutObj
          liftEffect $ addressObject.to_bech32 addressObj NoProblem.undefined

        addresses' <- for addresses \addrCborHex -> do
          _Address <- asksLib _."Address"
          bech32FromBytes (cborHexToCbor addrCborHex) NoProblem.undefined

        pure $ Array.nub $ utxoAddresses' <> addresses'
    _, _ -> do
      pure []

initialJson :: String
initialJson = String.joinWith "\n"
  [ "{"
  , """ "contractType": "PAM", """
  , """ "contractID": "pam01", """
  , """ "statusDate": "2023-01-31T00:00:00", """
  , """ "contractDealDate": "2023-01-28T00:00:00", """
  , """ "currency": "", """
  , """ "notionalPrincipal": "1000", """
  , """ "initialExchangeDate": "2023-02-01T00:00:00", """
  , """ "maturityDate": "2023-02-28T00:00:00", """
  , """ "nominalInterestRate": "0.1", """
  , """ "cycleAnchorDateOfInterestPayment": "2023-02-01T00:00:00", """
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

addressInput :: CardanoMultiplatformLib.Lib -> String -> Maybe String -> FormBuilder Effect Bech32
addressInput cardanoMultiplatformLib initial name = do
  let
    props =
      { initial
      , label: Just $ DOOM.text "Address"
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
  partyAddress <- addressInput cardanoMultiplatformLib "" $ Just "party"
  counterPartyAddress <- addressInput cardanoMultiplatformLib initialAddress $ Just "counter-party"
  let
    counterParty = bech32ToParty counterPartyAddress
    party = bech32ToParty partyAddress
    contract = createContract party counterParty contractTerms
  in
    { contractTerms
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
          fields <#> \field -> mb3 field
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

