module Component.CreateContract.SecondStep where

import Prelude

import Actus.Domain (CT(..), ContractTerms)
import Actus.Domain as A
import Actus.Domain.ContractTerms (mkContractTerms)
import Component.CreateContract.Types (AmortizingLoanChoice(..), ContractFormTypeChoice(..))
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, ChoiceFieldChoices(..), FormBuilder, FormBuilder', UseChoiceField(..), choiceField, choiceField', dateInput, dateTimeField, decimalInput, formBuilder, intInput, renderMultiField, selectFieldChoice, timeInput, unFormBuilder)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array ((..))
import Data.Array as Array
import Data.Array.ArrayAL as ArrayAL
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.FormURLEncoded.Query (Query)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Tuple.Nested ((/\))
import Data.Undefined.NoProblem (opt)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Polyform.Batteries as Batteries
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Validator (liftFnEither) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component)
import React.Basic.Hooks as React

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

contractTypeChoiceField :: FormBuilder' Effect CT
contractTypeChoiceField = do
  let
    choiceConfig a =
      { label: DOOM.text $ amortizingLoanChoiceToLabel a
      , helpText: Just $ DOOM.text $ amortizingLoanChoiceToHelpText a
      , disabled: false
      }
    useField = UseRadioButtonField choiceConfig
  amortizingLoanToContractType <$> choiceField' useField Nothing { label: Just $ DOOM.text "Amortization type" }

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

businessDayConventionField :: FormBuilder' Effect A.BDC
businessDayConventionField = do
  let
    choiceConfig a =
      { label: businessDayConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    { label: Just $ DOOM.text "Business day convention" }

calendarChoiceToLabel :: A.Calendar -> String
calendarChoiceToLabel = case _ of
  A.CLDR_NC -> "No calendar"
  A.CLDR_MF -> "Monday to Friday"

endOfMonthConventionChoiceToLabel :: A.EOMC -> String
endOfMonthConventionChoiceToLabel = case _ of
  A.EOMC_EOM -> "End of month"
  A.EOMC_SD -> "Same day"

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

contractRoleField :: FormBuilder' Effect A.CR
contractRoleField = do
  let
    choiceConfig a =
      { label: contractRoleChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    (Just $ ArrayAL.solo' A.CR_RPA [ A.CR_RPL ])
    { label: Just $ DOOM.text "Contract role" }

-- |DayCountConvention
-- data DCC
--   = DCC_A_AISDA -- ^ Actual/Actual ISDA
--   | DCC_A_360 -- ^ Actual/360
--   | DCC_A_365 -- ^ Actual/365
--   | DCC_E30_360ISDA -- ^ 30E/360 ISDA
--   | DCC_E30_360 -- ^ 30E/360
--   | DCC_B_252 -- ^ Business / 252
dayCountConventionChoiceToLabel :: A.DCC -> String
dayCountConventionChoiceToLabel = case _ of
  A.DCC_A_AISDA -> "Actual/Actual ISDA"
  A.DCC_A_360 -> "Actual/360"
  A.DCC_A_365 -> "Actual/365"
  A.DCC_E30_360ISDA -> "30E/360 ISDA"
  A.DCC_E30_360 -> "30E/360"
  A.DCC_B_252 -> "Business / 252"

dayCountConventionField :: FormBuilder' Effect A.DCC
dayCountConventionField = do
  let
    choiceConfig a =
      { label: dayCountConventionChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    { label: Just $ DOOM.text "Day count convention" }

-- FIXME: Implemnt cycle widget
-- -- |CyclePeriod
-- data Period
--   = P_D -- ^ Day
--   | P_W -- ^ Week
--   | P_M -- ^ Month
--   | P_Q -- ^ Quarter
--   | P_H -- ^ Half year
--   | P_Y -- ^ Year
cyclePeriodChoiceToLabel :: A.Period -> String
cyclePeriodChoiceToLabel = case _ of
  A.P_D -> "Day"
  A.P_W -> "Week"
  A.P_M -> "Month"
  A.P_Q -> "Quarter"
  A.P_H -> "Half year"
  A.P_Y -> "Year"

--- cyclePeriodField :: _ -> FormBuilder' Effect A.Period
cyclePeriodField props = do
  let
    choiceConfig a =
      { label: cyclePeriodChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    props

-- -- |CycleStub
-- data Stub
--   = ShortStub -- ^ Short last stub
--   | LongStub -- ^ Long last stub
cycleStubChoiceToLabel :: A.Stub -> String
cycleStubChoiceToLabel = case _ of
  A.ShortStub -> "Short last stub"
  A.LongStub -> "Long last stub"

-- cycleStubField :: forall m. Monad m => _ -> FormBuilder' m A.Stub
cycleStubField props = do
  let
    choiceConfig a =
      { label: cycleStubChoiceToLabel a
      , helpText: Nothing
      , disabled: false
      }
  choiceField'
    (UseSelectField choiceConfig)
    Nothing
    props

type Result = ContractTerms

cycleField label = formBuilder do
  form <- unFormBuilder $ ado
    n <- do
      let
        choices = SelectFieldChoices $ ArrayAL.solo' 1 (2 .. 31) <#> \i -> selectFieldChoice (show i) (show i)
        choiceConfig =
          { choices
          , validator: requiredV' $ Batteries.stringifyValidator Batteries.Int.validator
          , inline: true
          , initial: "1"
          }
      choiceField choiceConfig

    p <- cyclePeriodField { inline: true }
    stub <- cycleStubField { inline: true }
    in { n, p, stub, includeEndDay: false }

  pure $ renderMultiField label $ form

scheduleConfigField :: FormBuilder' _ _
scheduleConfigField = formBuilder do
  let
    calendarField :: FormBuilder' Effect A.Calendar
    calendarField = do
      let
        choiceConfig a =
          { label: calendarChoiceToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true }

    -- { label: Just $ DOOM.text "Calendar" }

    businessDayConventionField :: FormBuilder' Effect A.BDC
    businessDayConventionField = do
      let
        choiceConfig a =
          { label: businessDayConventionChoiceToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true }

    -- { label: Just $ DOOM.text "Business day convention" }

    endOfMonthConventionField :: FormBuilder' Effect A.EOMC
    endOfMonthConventionField = do
      let
        choiceConfig a =
          { label: endOfMonthConventionChoiceToLabel a
          , helpText: Nothing
          , disabled: false
          }
      choiceField'
        (UseSelectField choiceConfig)
        Nothing
        { inline: true }
  -- { label: Just $ DOOM.text "End of month convention" }

  form <- unFormBuilder $ ado
    calendar <- calendarField
    endOfMonthConvention <- endOfMonthConventionField
    businessDayConvention <- businessDayConventionField
    in
      { calendar
      , businessDayConvention
      , endOfMonthConvention
      }

  pure $ renderMultiField (Just $ DOOM.text "Schedule config") $ form

mkAmortizingLoanForm :: BootstrapForm Effect Query Result
mkAmortizingLoanForm = FormBuilder.evalBuilder ado
  contractId <- do
    let
      props =
        { initial: ""
        , label: Just $ DOOM.text "Contract ID"
        , validator: requiredV' $ identity
        , helpText: Nothing
        }
    FormBuilder.textInput props
  contractType <- contractTypeChoiceField
  { businessDayConvention, calendar, endOfMonthConvention } <- scheduleConfigField
  dayCountConvention <- dayCountConventionField
  contractRole <- contractRoleField

  let
    cycleOfInterestPayment =
      { n: 1
      , p: A.P_Y
      , stub: A.LongStub
      , includeEndDay: false
      }
    -- FIXME: Implement currency choice
    currency = "DjedTestUSD"

  -- initialExchangeDate = "2024-01-01T00:00:00", """
  cycleOfInterestPayment <- cycleField (Just $ DOOM.text "Cycle of interest payment")

  initialExchangeDate <- dateTimeField $ Just $ DOOM.text "Initial exchange date"

  cycleAnchorDateOfInterestPayment <- dateTimeField $ Just $ DOOM.text "Cycle anchor date of interest payment"

  statusDate <- dateTimeField $ Just $ DOOM.text "Status date"

  maturityDate <- dateTimeField $ Just $ DOOM.text "Maturity date"

  nominalInterestRate <- decimalInput
    { label: Just $ DOOM.text "Nominal interest rate" }

  notionalPrincipal <- decimalInput
    { label: Just $ DOOM.text "Notional principal" }

  premiumDiscountAtIED <- decimalInput
    { label: Just $ DOOM.text "Premium discount at IED" }

  rateMultiplier <- decimalInput
    { label: Just $ DOOM.text "Rate multiplier" }

  in
    mkContractTerms
      { contractType
      , contractId
      , dayCountConvention: Just dayCountConvention
      , statusDate: statusDate
      , scheduleConfig:
          { calendar: Just calendar
          , businessDayConvention: Just businessDayConvention
          , endOfMonthConvention: Just endOfMonthConvention
          }
      , contractRole
      , cycleOfInterestPayment: Just cycleOfInterestPayment
      , initialExchangeDate: Just initialExchangeDate
      , cycleAnchorDateOfInterestPayment: Just cycleAnchorDateOfInterestPayment
      , maturityDate: Just maturityDate
      , nominalInterestRate: Just nominalInterestRate
      , notionalPrincipal: Just notionalPrincipal
      , premiumDiscountAtIED: Just premiumDiscountAtIED
      , rateMultiplier: Just rateMultiplier
      }

-- cp1 : {address: {…}}
-- pa1 : {address: {…}}

-- bdc : "NULL"
-- cal : "NC"
-- emoc : "SD"
-- cid : "Final Demo Test"
-- cntrl : "RPA"
-- ct : "PAM"

-- dcc : "30E360"
-- ipcl : "P1YL0"
-- cur : "DjedTestUSD"

-- ied : 1704067200000
-- ipanx : 1735689600000
-- md : 1735689600000
-- ipnr : "0.1"
-- nt : "5"
-- pdied : "0"
-- rrmlt : "1"

mkJsonForm :: _ -> BootstrapForm Effect Query Result
mkJsonForm cardanoMultiplatformLib = FormBuilder.evalBuilder $ FormBuilder.textArea
  { missingError: "Please provide contract terms JSON value"
  , initial: initialJson
  , validator: requiredV' $ Validator.liftFnEither \jsonString -> do
      json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
      lmap (Array.singleton <<< show) (decodeJson json)
  , rows: 15
  , name: (Just $ "contract-terms")
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
        Just (V (Right contractFormTypeChoice) /\ _) -> do
          onSuccess contractFormTypeChoice
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit
      form = case contractFormTypeChoice of
        AmortizingLoans -> mkAmortizingLoanForm
        _ -> mkJsonForm cardanoMultiplatformLib

    { formState, onSubmit: onSubmit', result } <- useForm
      { spec: form
      , onSubmit
      , validationDebounce: Seconds 0.5
      }

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
                    Just (V (Right _) /\ _) -> false
                    _ -> true
                { className: "btn btn-primary"
                , onClick: onSubmit'
                , disabled
                }
              [ R.text "Submit" ]
          ]

      if inModal then modal
        { title: R.text "Add contract | Step 2 of 4"
        , onDismiss
        , body: DOM.div { className: "row" }
            [ DOM.div { className: "col-9" } [ formBody ]
            , DOM.div { className: "col-3" } [ DOOM.text "TEST" ]
            ]
        , footer: formActions
        , size: Modal.ExtraLarge
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

