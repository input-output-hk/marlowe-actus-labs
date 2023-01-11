module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Address (Bech32(..), bech32FromHex, isValidBech32)
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
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (defaultRiskFactors, genContract)
import Marlowe.Runtime.Web.Types (Address(..), addressToParty) as RT
import Marlowe.Runtime.Web.Types (Address, TxOutRef)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMEither) as Validator
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

  , changeAddress :: RT.Address
  , usedAddresses :: Array RT.Address
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

addressInput :: CardanoMultiplatformLib.Lib -> String -> Maybe String -> FormBuilder Effect Address
addressInput cardanoMultiplatformLib initial name = do
  let
    props =
      { initial
      , label: Just $ DOOM.text "Address"
      , name
      , validator: Validator.liftFnMEither \str -> do
          isValidBech32 cardanoMultiplatformLib str <#>
            if _ then Right (RT.Address str)
            else Left [ "Invalid address" ]
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
    counterParty = RT.addressToParty counterPartyAddress
    party = RT.addressToParty partyAddress
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
          let
            walletAddressToBech32 (Wallet.Address addressStr) = do
              bech32FromHex cardanoMultiplatformLib addressStr >>= case _ of
                Just (Bech32 bech32) -> pure $ Just $ RT.Address bech32
                Nothing -> pure Nothing

          possibleAddress <- Wallet.getChangeAddress wallet
          possibleUsedAddresses <- Wallet.getUsedAddresses wallet
          case possibleAddress, possibleUsedAddresses of
            Right address, Right addresses -> do
              -- setChangeAddresses $ Just addresses
              addr <- liftEffect $ walletAddressToBech32 address
              liftEffect $ case addr, Map.lookup (FieldId "party") formState.fields of
                Just (RT.Address addr), Just { onChange } ->
                  onChange [ addr ]
                _, _ -> do
                  pure unit

              liftEffect $ do
                addresses' <- Array.catMaybes <$> for addresses \address -> do
                  walletAddressToBech32 address
                setChangeAddresses $ Just addresses'
            _, _ -> do
              traceM "Failed to get change address"
              traceM possibleAddress
              traceM "Used addresses"
              traceM possibleUsedAddresses
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
