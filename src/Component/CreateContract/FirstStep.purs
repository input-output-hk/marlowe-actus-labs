module Component.CreateContract.FirstStep where

import Prelude

import Component.CreateContract.Types (ContractFormTypeChoice(..))
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM)
import Component.Widgets (link)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (BootstrapForm, ChoiceFieldChoices(..), FormBuilder, choiceField)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Either (Either(..))
import Data.Enum (upFromIncluding)
import Data.FormURLEncoded.Query (Query)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded.Duals as UrlEncoded.Duals
import Polyform.Dual as Polyform.Dual
import Polyform.Validator.Dual as Duals
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component)
import React.Basic.Hooks as React
import Type.Prelude (Proxy(..))

type Result = ContractFormTypeChoice

contractFormTypeChoiceToLabel :: ContractFormTypeChoice -> String
contractFormTypeChoiceToLabel AmortizingLoans = "Amortizing Loans"
contractFormTypeChoiceToLabel Bonds = "Bonds"
contractFormTypeChoiceToLabel BulletLoans = "Bullet Loans"
contractFormTypeChoiceToLabel CapitalizingLoans = "Capitalizing Loans"
contractFormTypeChoiceToLabel ZeroCouponBonds = "Zero Coupon Bonds"
contractFormTypeChoiceToLabel JsonForm = "Any ACUTS contract (Json Form)"

contractFormTypeChoiceToHelpText :: ContractFormTypeChoice -> JSX
contractFormTypeChoiceToHelpText AmortizingLoans = DOOM.text "An amortizing loan is a loan where according to an amortization schedule payments are executed to pay back the principal with interest."
contractFormTypeChoiceToHelpText JsonForm = DOOM.text "You can create any contract by providing JSON object which is conformant to the ACTUS specification."
contractFormTypeChoiceToHelpText _ = DOOM.text "Not implemented yet."

contractFormTypeChoiceField :: FormBuilder Effect ContractFormTypeChoice
contractFormTypeChoiceField = do
  let
    dual = Batteries.stringifyDual $ UrlEncoded.Duals.enum (Proxy :: Proxy ContractFormTypeChoice)

    serialize ∷ ContractFormTypeChoice → String
    serialize = Duals.runSerializer dual

    validator = Polyform.Dual.parser dual

    asChoice a = do
      let
        value = serialize a
        label = DOOM.text $ contractFormTypeChoiceToLabel a
        disabled = not $ a `Array.elem` [ JsonForm, AmortizingLoans ]
        helpText = Just $ contractFormTypeChoiceToHelpText a
      { label, value, disabled, helpText }
    choices = RadioButtonFieldChoices
      { switch: true
      , choices: map asChoice (upFromIncluding bottom ∷ ArrayAL 1 ContractFormTypeChoice)
      }
  choiceField { choices, validator }

type Props =
  { onSuccess :: Result -> Effect Unit
  -- , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  }

form :: BootstrapForm Effect Query Result
form = FormBuilder.evalBuilder contractFormTypeChoiceField

mkComponent :: MkComponentM (Props -> JSX)
mkComponent = do
  modal <- liftEffect mkModal

  liftEffect $ component "CreateContract.FirstStep" \{ onSuccess, onDismiss, inModal } -> React.do
    let
      onSubmit = _.result >>> case _ of
        Just (V (Right contractFormTypeChoice)) -> do
          onSuccess contractFormTypeChoice
        _ -> do
          -- Rather improbable path because we disable submit button if the form is invalid
          pure unit

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

