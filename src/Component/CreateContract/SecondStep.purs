module Component.CreateContract.SecondStep where

import Prelude
import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Core (genProjectedCashflows)
import Actus.Domain (CT(..), ContractTerms(..), DCC)
import Actus.Domain (ContractTerms)
import Actus.Domain as A
import Actus.Domain.ContractTerms (ContractTermsRow)
import CardanoMultiplatformLib (Bech32, bech32FromBytes, bech32FromString, bech32ToString, runGarbageCollector)
import CardanoMultiplatformLib (Bech32, bech32FromBytes, bech32FromString, bech32ToString, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (cborHexToCbor)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.CreateContract.Types (AmortizingLoanChoice(..), ContractFormTypeChoice(..))
import Component.Modal (mkModal)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Component.Widgets (link)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, FormBuilder, UseChoiceField(..), choiceField')
import Contrib.React.Bootstrap.FormBuilder (FormBuilder, BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array (elem, fromFoldable)
import Data.Array (elem, fromFoldable)
import Data.Array as Array
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (fromInt, toNumber, toString)
import Data.BigInt.Argonaut (fromInt, toNumber, toString)
import Data.DateTime (DateTime(..))
import Data.Decimal (Decimal)
import Data.Either (Either(..), hush)
import Data.Either (Either(..), hush)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Formatter.DateTime (formatDateTime)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map as Map
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Time.Duration (Seconds(..))
import Data.Undefined.NoProblem as NoProblem
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Debug (traceM)
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party, Value(..))
import Language.Marlowe.Core.V1.Semantics.Types (Party, Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows, currenciesWith6Decimals, defaultRiskFactors, evalVal, genContract, toMarloweCashflow)
import Marlowe.Actus (CashFlows, currenciesWith6Decimals, defaultRiskFactors, evalVal, genContract, toMarloweCashflow)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
import Polyform.Batteries (rawError)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks as React
import Record as Record
import Wallet as Wallet
import Wallet as Wallet
import WalletContext (walletAddresses)
import WalletContext (walletAddresses)

amortizingLoanChoiceToLabel :: AmortizingLoanChoice -> String
amortizingLoanChoiceToLabel = case _ of
  PrincipalAtMaturity -> "Principal at maturity"
  LinearAmortizer -> "Linear amortizer"
  NegativeAmortizer -> "Negative amortizer"
  Annuity -> "Annuity"

amortizingLoanChoiceToHelpText :: AmortizingLoanChoice -> String
amortizingLoanChoiceToHelpText = case _ of
  PrincipalAtMaturity -> "Principal at maturity only defines periodic interest payments, the full principal is due at maturity."
  LinearAmortizer -> "Regular principal repayments over time, the interest payments decrease linearly."
  NegativeAmortizer -> "Negative amortization means that the payments per period are smaller than the interest, i.e. the balance of the loan increases over time."
  Annuity -> "The annuity amortization consists of regular payments of equal amounts over the lifetime of the loan."

amortizingLoanToContractType :: AmortizingLoanChoice -> CT
amortizingLoanToContractType = case _ of
  PrincipalAtMaturity -> PAM
  LinearAmortizer -> LAM
  NegativeAmortizer -> NAM
  Annuity -> ANN

contractTypeChoiceField :: FormBuilder Effect CT
contractTypeChoiceField = do
  let
    choiceConfig a =
      { label: DOOM.text $ amortizingLoanChoiceToLabel a
      , helpText: Just $ DOOM.text $ amortizingLoanChoiceToHelpText a
      , disabled: false
      }
    useField = UseRadioButtonField choiceConfig
  amortizingLoanToContractType <$> choiceField' useField { label: Just $ DOOM.text "Amortization type" }

businessDayConventionChoiceToLabel :: A.BDC -> String
businessDayConventionChoiceToLabel = case _ of
  A.BDC_NULL -> "No shift"
  A.BDC_SCF -> "Shift/calculate following"
  A.BDC_SCMF -> "Shift/calculate modified following"
  A.BDC_CSF -> "Calculate/shift following"
  A.BDC_CSMF -> "Calculate/shift modified following"
  A.BDC_SCP -> "Shift/calculate preceding"
  A.BDC_SCMP -> "Shift/calculate modified preceding"
  A.BDC_CSP -> "Calculate/shift preceding"
  A.BDC_CSMP -> "Calculate/shift modified preceding"

businessDayConventionField :: FormBuilder Effect A.BDC
businessDayConventionField = do
  let
    choiceConfig a =
      { label: businessDayConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField' (UseSelectField choiceConfig) { label: Just $ DOOM.text "Business day convention" }

calendarChoiceToLabel :: A.Calendar -> String
calendarChoiceToLabel = case _ of
  A.CLDR_NC -> "No calendar"
  A.CLDR_MF -> "Monday to Friday"

calendarField :: FormBuilder Effect A.Calendar
calendarField = do
  let
    choiceConfig a =
      { label: calendarChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField' (UseSelectField choiceConfig) { label: Just $ DOOM.text "Calendar" }

endOfMonthConventionChoiceToLabel :: A.EOMC -> String
endOfMonthConventionChoiceToLabel = case _ of
  A.EOMC_EOM -> "End of month"
  A.EOMC_SD -> "Same day"

endOfMonthConventionField :: FormBuilder Effect A.EOMC
endOfMonthConventionField = do
  let
    choiceConfig a =
      { label: endOfMonthConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField' (UseSelectField choiceConfig) { label: Just $ DOOM.text "End of month convention" }

-- -- |ContractRole
-- data CR
--   = CR_RPA -- ^ Real position asset
--   | CR_RPL -- ^ Real position liability
--   | CR_CLO -- ^ Role of a collateral
--   | CR_CNO -- ^ Role of a close-out-netting
--   | CR_COL -- ^ Role of an underlying to a collateral
--   | CR_LG -- ^ Long position
--   | CR_ST -- ^ Short position
--   | CR_BUY -- ^ Protection buyer
--   | CR_SEL -- ^ Protection seller
--   | CR_RFL -- ^ Receive first leg
--   | CR_PFL -- ^ Pay first leg
--   | CR_RF -- ^ Receive fix leg
--   | CR_PF -- ^ Pay fix leg
contractRoleChoiceToLabel :: A.CR -> String
contractRoleChoiceToLabel = case _ of
  A.CR_RPA -> "Real position asset"
  A.CR_RPL -> "Real position liability"
  A.CR_CLO -> "Role of a collateral"
  A.CR_CNO -> "Role of a close-out-netting"
  A.CR_COL -> "Role of an underlying to a collateral"
  A.CR_LG -> "Long position"
  A.CR_ST -> "Short position"
  A.CR_BUY -> "Protection buyer"
  A.CR_SEL -> "Protection seller"
  A.CR_RFL -> "Receive first leg"
  A.CR_PFL -> "Pay first leg"
  A.CR_RF -> "Receive fix leg"
  A.CR_PF -> "Pay fix leg"

contractRoleField :: FormBuilder Effect A.CR
contractRoleField = do
  let
    choiceConfig a =
      { label: contractRoleChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    { label: Just $ DOOM.text "Contract role" }

type Result =
  { -- contractTerms :: ContractTerms
    businessDayConvention :: A.BDC
  , contractType :: A.CT
  , contractId :: String
  , calendar :: A.Calendar
  , contractRole :: A.CR
  , endOfMonthConvention :: A.EOMC
  }

mkAmortizingLoanForm :: BootstrapForm Effect Query Result
mkAmortizingLoanForm = FormBuilder.evalBuilder ado
  contractId <- do
    let
      props =
        { initial: ""
        , label: Just $ DOOM.text "Contract ID"
        , validator: identity
        , helpText: Nothing
        }
    FormBuilder.textInput props
  contractType <- contractTypeChoiceField
  businessDayConvention <- businessDayConventionField
  calendar <- calendarField
  endOfMonthConvention <- endOfMonthConventionField
  contractRole <- contractRoleField
  in
    { calendar
    , contractType
    , contractId
    , businessDayConvention
    , endOfMonthConvention
    , contractRole
    }

mkJsonForm :: _ -> BootstrapForm Effect Query Result
mkJsonForm cardanoMultiplatformLib = FormBuilder.evalBuilder ado
  (contractTerms :: ContractTerms) <- FormBuilder.textArea
    { missingError: "Please provide contract terms JSON value"
    , initial: initialJson
    , validator: Validator.liftFnEither \jsonString -> do
        json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
        lmap (Array.singleton <<< show) (decodeJson json)
    , rows: 15
    , name: (Just $ "contract-terms")
    }
  -- let
  --   cashFlows =
  --     genProjectedCashflows
  --       (party /\ counterParty)
  --       (defaultRiskFactors contractTerms)
  --       contractTerms
  --   contract = genContract cashFlows
  in
    { -- contractTerms
      contractType: A.PAM
    , businessDayConvention: A.BDC_NULL
    , calendar: A.CLDR_NC
    , contractId: "Contract"
    , contractRole: A.CR_RPA
    , endOfMonthConvention: A.EOMC_EOM
    }


type Props =
  { contractFormTypeChoice :: ContractFormTypeChoice
  , onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  }

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  modal <- liftEffect mkModal
  cardanoMultiplatformLib <- asks _.cardanoMultiplatformLib

  liftEffect $ component "CreateContract.SecondStep" \{ contractFormTypeChoice, onSuccess, onDismiss, inModal } -> React.do
    let
      onSubmit = _.result >>> case _ of
        Just (V (Right contractFormTypeChoice)) -> do
          onSuccess contractFormTypeChoice
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit
      form = case contractFormTypeChoice of
        AmortizingLoans -> mkAmortizingLoanForm
        _ -> mkJsonForm cardanoMultiplatformLib

    { formState, onSubmit: onSubmit', result } <- useForm { spec: form, onSubmit, validationDebounce: Seconds 0.5 }

    pure $ do
      let
        fields = UseForm.renderForm form formState
        formBody = DOM.div { className: "form-group" } fields
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



-- bdc : "NULL"
-- cal : "NC"
-- emoc : "SD"
-- cid : "Final Demo Test"

-- cntrl : "RPA"
-- cp1 : {address: {…}}
-- ct : "PAM"
-- cur : "DjedTestUSD"
-- dcc : "30E360"
-- ied : 1704067200000
-- ipanx : 1735689600000
-- ipcl : "P1YL0"
-- ipnr : "0.1"
-- md : 1735689600000
-- nt : "5"
-- pa1 : {address: {…}}
-- pdied : "0"
-- rrmlt : "1"


