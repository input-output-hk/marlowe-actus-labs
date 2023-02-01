module Contrib.React.Bootstrap.FormBuilder where

import Prelude

import Contrib.React.Basic.Hooks.UseForm (Form, InputState)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.Form (select)
import Contrib.React.Bootstrap.Form as Form
import Contrib.React.Bootstrap.Form.Check as Check
import Contrib.React.Bootstrap.Form.Control as Form.Control
import Control.Monad.State (State, evalState, get, put)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, upFromIncluding)
import Data.Foldable (fold, foldMap, null, traverse_)
import Data.Foldable (length, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.Functor.Compose (Compose(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid as Monoid
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Seconds)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Random (random)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1)
import Polyform as Polyform
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Duals as UrlEncoded.Duals
import Polyform.Batteries.UrlEncoded.Types (stringifyValidator)
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Dual as Polyform.Dual
import Polyform.Validator (runValidator)
import Polyform.Validator as Validator
import Polyform.Validator.Dual as Polyform.Validator.Dual
import Prim.Row as Row
import React.Basic (JSX)
import React.Basic (JSX)
import React.Basic as DOOM
import React.Basic.DOM as DOOM
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, SyntheticEvent, handler, handler_)
import React.Basic.Events (handler)
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseState, component, useEffect, useEffectOnce, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import Record as Record
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Utils.React.Basic.Hooks (useDebounce)

-- The current default rendering uses plain JSX and not react-bootstrap
-- for labels and other pieces beside `Form.Control`.
-- TODO: validate if the migration to react-bootstrap is worth it.
type FormElement = JSX

-- TODO [optimization]: we can replace `Array` in here by the array-builder...
type BootstrapForm m = Form m (Array FormElement) String

type IdCounter = Int
type FormBuilder m a = Compose (State IdCounter) (BootstrapForm m Query) a

evalBuilder :: forall m a. FormBuilder m a -> BootstrapForm m Query a
evalBuilder (Compose m) = evalState m 0

_getIdentifiers
  :: forall r. { id :: Maybe String, name :: Maybe String | r } -> State IdCounter { id :: String, name :: String }
_getIdentifiers props = do
  case props.id, props.name of
    Just id, Just name -> pure { id, name }
    Just id, Nothing -> pure { id, name: id }
    Nothing, Just name -> do
      counter <- get
      put (counter + 1)
      pure { id: "id-" <> show counter, name }
    Nothing, Nothing -> do
      counter <- get
      put (counter + 1)
      let id = "id-" <> show counter
      pure { id, name: id }

type OptTextInputOptionalPropsRow r =
  ( label :: Maybe JSX
  , id :: Maybe String
  , name :: Maybe String
  , initial :: String
  , helpText :: Maybe JSX
  , placeholder :: String
  | r
  )

type TextInputOptionalPropsRow r =
  ( missingError :: String
  | OptTextInputOptionalPropsRow r
  )

type TextInputOptionalProps = { | TextInputOptionalPropsRow () }

defaultTextInputProps :: TextInputOptionalProps
defaultTextInputProps =
  { label: Nothing
  , id: Nothing
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , placeholder: ""
  , helpText: Nothing
  }

type TextInputProps m a =
  { validator :: Batteries.Validator m String String a
  | TextInputOptionalPropsRow ()
  }

-- Rendering helper used by fields constructors below.
-- Currently we are following the form layout described here:
--  https://getbootstrap.com/docs/5.0/forms/layout/#horizontal-form
-- TODO: Make it a default prop of the final field
-- constructor.
renderTextInput
  :: { id :: String
     , possibleLabel :: Maybe JSX
     , name :: String
     , placeholder :: String
     }
  -> InputState String
  -> FormElement
renderTextInput { id, possibleLabel, name, placeholder } { value, errors, onChange, touched } = do
  let
    label = DOM.label
      { className: "col-form-label-sm col-sm-3", htmlFor: id } $ fold possibleLabel
    body =
      DOM.div { className: "col-sm-9"} $
        Form.textInput
          { id
          , name
          , placeholder
          , value
          , onChange: handler targetValue (onChange <<< fromMaybe "")
          , isValid: maybe false null errors
          , isInvalid: touched && maybe false (not <<< null) errors
          }
          <> do
            let
              errors' :: Array String
              errors' = fold errors
            Monoid.guard (touched && not (null errors')) $
              DOM.div { className: "invalid-feedback" }
                [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]
  DOM.div { className: "mb-3 row" } [ label, body ]

textInput
  :: forall m props a
   . Monad m
  => Defaults TextInputOptionalProps { | props } (TextInputProps m a)
  => { | props }
  -> FormBuilder m a
textInput props = Compose do
  { id, name } <- _getIdentifiers props'
  let
    form :: BootstrapForm m Query a
    form = UseForm.input
      (FieldId name)
      props'.initial
      props'.missingError
      ( Array.singleton <<< renderTextInput
          { id, possibleLabel: props'.label, name, placeholder: props'.placeholder }
      )
      props'.validator
  pure form
  where
  props' = defaults defaultTextInputProps props

type OptTextInputOptionalProps = { | OptTextInputOptionalPropsRow () }

defaultOptTextInputProps :: OptTextInputOptionalProps
defaultOptTextInputProps =
  { label: Nothing
  , id: Nothing
  , initial: ""
  , name: Nothing
  , placeholder: ""
  , helpText: Nothing
  }

type OptTextInputProps m a =
  { validator :: Batteries.Validator m String String a
  | OptTextInputOptionalPropsRow ()
  }

optTextInput
  :: forall m props a
   . Monad m
  => Defaults OptTextInputOptionalProps { | props } (OptTextInputProps m a)
  => { | props }
  -> FormBuilder m (Maybe a)
optTextInput props = Compose do
  { id, name } <- _getIdentifiers props'
  let
    render = Array.singleton <<< renderTextInput
      { id, possibleLabel: props'.label, name, placeholder: props'.placeholder }

    form :: BootstrapForm m Query (Maybe a)
    form = UseForm.optInput
      (FieldId name)
      props'.initial
      render
      props'.validator
  pure form
  where
  props' = defaults defaultOptTextInputProps props

type TextAreaOptionalPropsRow r =
  ( rows :: Int
  | TextInputOptionalPropsRow r
  )

type TextAreaOptionalProps = { | TextAreaOptionalPropsRow () }

defaultTextAreaProps :: TextAreaOptionalProps
defaultTextAreaProps =
  { label: Nothing
  , id: Nothing
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , placeholder: ""
  , rows: 3
  , helpText: Nothing
  }

type TextAreaProps m a =
  { validator :: Batteries.Validator m String String a
  | TextAreaOptionalPropsRow ()
  }

-- TODO: This function is nearly identical to `renderTextInput` above.
-- We should find a way to smash them into a single one by introducing:
-- `data AnyTextInput = TextArea { rows: Int } | TextInput`.
renderTextArea
  :: { id :: String
     , helpText :: Maybe JSX
     , possibleLabel :: Maybe JSX
     , name :: String
     , placeholder :: String
     , rows :: Int
     }
  -> InputState String
  -> FormElement
renderTextArea { id, possibleLabel, helpText, name, placeholder, rows } { value, errors, onChange, touched } = do
  let
    label = DOM.label { className: "col-form-label-sm col-sm-3", htmlFor: id } $ possibleLabel `flip foldMap` \labelJsx ->
      [ labelJsx ]
    body = DOM.div { className: "col-sm-9" } $
      Form.Control.textArea
        { id
        , name
        , placeholder
        , value
        , onChange: handler targetValue (onChange <<< fromMaybe "")
        , rows
        , isValid: maybe false null errors
        , isInvalid: touched && maybe false (not <<< null) errors
        }
        <> do
          let
            errors' :: Array String
            errors' = fold errors

            showErrors = touched && not (null errors')

          fold
            [ Monoid.guard (touched && not (null errors')) $
                DOM.div { className: "invalid-feedback" }
                  [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]

            , Monoid.guard (isJust helpText && not showErrors) $
                DOM.div { className: "form-text" }
                  [ fold helpText ]
            ]
  DOM.div { className: "mb-3 row" } [ label, body ]

textArea
  :: forall m props a
   . Monad m
  => Defaults TextAreaOptionalProps { | props } (TextAreaProps m a)
  => { | props }
  -> FormBuilder m a
textArea props = Compose do
  { id, name } <- _getIdentifiers props'
  let
    form :: BootstrapForm m Query a
    form = UseForm.input
      (FieldId name)
      props'.initial
      props'.missingError
      ( Array.singleton <<< renderTextArea
          { id
          , possibleLabel: props'.label
          , name
          , helpText: props'.helpText
          , placeholder: props'.placeholder
          , rows: props'.rows
          }
      )
      props'.validator
  pure form
  where
  props' = defaults defaultTextAreaProps props

-- Help texts can be specified for each choice
-- but also for the whole field.
type FieldChoice label =
  { disabled :: Boolean
  , helpText :: Maybe JSX
  , label :: label
  , value :: String
  }

type RadioFieldChoice = FieldChoice JSX

radioFieldChoice :: String -> JSX -> RadioFieldChoice
radioFieldChoice value label = { disabled: false, helpText: Nothing, label, value }

type SelectFieldChoice = FieldChoice String

selectFieldChoice :: String -> String -> SelectFieldChoice
selectFieldChoice label value = { disabled: false, helpText: Nothing, label, value }

data ChoiceFieldChoices
  = RadioButtonFieldChoices
      { switch :: Boolean
      , choices :: ArrayAL 1 RadioFieldChoice -- use `solo` / `solo'` to create
      }
  | SelectFieldChoices (ArrayAL 1 SelectFieldChoice) -- use `duet` / `duet'` to create

type ChoiceFieldOptionalPropsRow r =
  ( label :: Maybe JSX
  , helpText :: Maybe JSX
  , id :: Maybe String
  , missingError :: String
  , name :: Maybe String
  , initial :: String
  | r
  )

type ChoiceFieldOptionalProps = { | ChoiceFieldOptionalPropsRow () }

defaultChoiceFieldProps :: ChoiceFieldOptionalProps
defaultChoiceFieldProps =
  { label: Nothing
  , helpText: Nothing
  , id: Nothing
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  }

type ChoiceFieldProps m a =
  { choices :: ChoiceFieldChoices
  , validator :: Batteries.Validator m String String a
  | ChoiceFieldOptionalPropsRow ()
  }

renderChoiceField
  :: { choices :: ChoiceFieldChoices
     , id :: String
     , possibleLabel :: Maybe JSX
     , name :: String
     , possibleHelpText :: Maybe JSX
     }
  -> InputState String
  -> Array FormElement
renderChoiceField { choices, id, possibleHelpText, possibleLabel, name } { value: selectedValue, errors, onChange, touched } = do
  let
    label = DOM.legend { className: "col-form-label-sm col-sm-3" } $ possibleLabel `flip foldMap` \labelJsx ->
      [ labelJsx ]

    body = case choices of
      RadioButtonFieldChoices { switch, choices: choices' } -> do
        let
          renderChoice { disabled, helpText, label, value } = do
            let
              checked = value == selectedValue
              label' =
                label
                  <> do
                    helpText `flip foldMap` \helpText' -> do
                      DOOM.div
                        { className: "form-text text-muted mt-0 mb-2"
                        , children: [ helpText' ]
                        }

            Form.check
              { disabled
              , id: id <> "-" <> value
              , label: label'
              , isValid: maybe false null errors
              , isInvalid: touched && maybe false (not <<< null) errors
              , name
              -- , feedback
              -- , feedbackTooltip
              , "type":
                  if switch then Check.checkType.switch
                  else Check.checkType.radio
              , value
              , checked
              , onChange: handler_ do
                  when (not checked) $ onChange value
              }
        DOM.div { className: "col-sm-9 form-check" } $
          map renderChoice (ArrayAL.toArray choices')

      SelectFieldChoices choices' -> do
        let
          renderOption { disabled, label: label', value } = do
            DOM.option { value, disabled } [ DOOM.text label' ]

          onChangeHandler = handler targetValue \val -> do
            traceM val
            (traverse_ onChange val)

        -- choiceHelpText = 

        DOM.div { className: "col-sm-9" } $
          [ select
              { onChange: onChangeHandler
              , value: selectedValue
              , id
              , name
              , isValid: maybe false null errors
              , isInvalid: touched && maybe false (not <<< null) errors
              } $
              map renderOption (ArrayAL.toArray choices')
          ]
            <> possibleHelpText `flip foldMap` \helpText -> do
              [ DOOM.div
                  { className: "form-text text-muted mt-0 mb-2"
                  , children: [ helpText ]
                  }
              ]

  pure $ DOM.div { className: "row mb-3" } [ label, body ]

choiceField
  :: forall m props a
   . Monad m
  => Defaults ChoiceFieldOptionalProps { | props } (ChoiceFieldProps m a)
  => { | props }
  -> FormBuilder m a
choiceField props = Compose do
  { id, name } <- _getIdentifiers props'
  let
    form = UseForm.input
      (FieldId name)
      props'.initial
      props'.missingError
      ( renderChoiceField
          { id, possibleLabel: props'.label, name, choices: props'.choices, possibleHelpText: props'.helpText }
      )
      props'.validator
  pure form
  where
  props' = defaults defaultChoiceFieldProps props

type ChoiceConfig doc =
  { disabled :: Boolean
  , label :: doc
  , helpText :: Maybe JSX
  }

data UseChoiceField a
  = UseRadioButtonField
      (a -> ChoiceConfig JSX)
  | UseSelectField
      (a -> ChoiceConfig String)

choiceField'
  :: forall m props props' a
   . Monad m
  => BoundedEnum a
  => Defaults ChoiceFieldOptionalProps { | props' } (ChoiceFieldProps m a)
  => Row.Nub (choices :: ChoiceFieldChoices, initial :: String, validator :: Batteries.Validator m String String a | props) props'
  => UseChoiceField a
  -> { | props }
  -> FormBuilder m a
choiceField' useElement props = do
  let
    dual :: Polyform.Validator.Dual.Dual m _ _ _
    dual = Batteries.stringifyDual $ UrlEncoded.Duals.enum (Proxy :: Proxy a)
    validator = Polyform.Dual.parser dual

    serialize :: a -> String
    serialize = Polyform.Validator.Dual.runSerializer dual

    asChoice :: forall doc. (a -> ChoiceConfig doc) -> a -> FieldChoice doc
    asChoice mkCfg a = do
      let
        value = serialize a

        cfg :: ChoiceConfig doc
        cfg = mkCfg a

      { label: cfg.label
      , value
      , disabled: cfg.disabled
      , helpText: cfg.helpText
      }

    arr = upFromIncluding bottom :: ArrayAL 1 a
    initial = serialize (ArrayAL.head arr)

    choices = case useElement of
      UseRadioButtonField mkCfg -> RadioButtonFieldChoices
        { switch: true
        , choices: do
            let
              asChoice' = asChoice mkCfg
            map asChoice' arr
        }
      UseSelectField mkCfg -> SelectFieldChoices do
        let
          asChoice' = asChoice mkCfg
        map asChoice' arr

    props' :: { | props' }
    props' = Record.merge
      { choices
      , validator
      , initial
      }
      props
  choiceField props'
