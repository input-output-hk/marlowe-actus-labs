module Contrib.React.Bootstrap.FormBuilder where

import Prelude

import Contrib.Data.Foldable (foldMapFlipped)
import Contrib.Polyform.Batteries.UrlEncoded (requiredV')
import Contrib.React.Basic.Hooks.UseForm (Form(..), InputState)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.Form as Bootstrap.Form
import Contrib.React.Bootstrap.Form as Form
import Contrib.React.Bootstrap.Form.Check as Check
import Contrib.React.Bootstrap.Form.Control as Form.Control
import Control.Monad.State (State, evalState, get, put)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Array.ArrayAL (ArrayAL)
import Data.Array.ArrayAL as ArrayAL
import Data.Bifunctor (lmap)
import Data.Date (Date)
import Data.DateTime (DateTime(..), Hour, Minute, Time(..))
import Data.DateTime.ISO (parseISODate, parseISOTime)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum, upFromIncluding)
import Data.Foldable (class Foldable, fold, foldMap, foldl, null, traverse_)
import Data.Foldable (length, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.Formatter.Parser.Number (parseDigit)
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
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Random (random)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1)
import Parsing (fail)
import Parsing as Parsing
import Parsing.Combinators as Parsing
import Parsing.String as Parsing
import Polyform as Polyform
import Polyform.Batteries as Batteries
import Polyform.Batteries.Decimal as Batteries.Decimal
import Polyform.Batteries.Int as Batteries.Int
import Polyform.Batteries.Number as Batteries.Number
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Duals as UrlEncoded.Duals
import Polyform.Batteries.UrlEncoded.Types (stringifyValidator)
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Batteries.UrlEncoded.Validators (MissingValue)
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Dual as Polyform.Dual
import Polyform.Validator (liftFnEither, runValidator)
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
-- TODO: check if the migration to react-bootstrap is worth it.
type FormElement = JSX

-- TODO [optimization]: we can replace `Array` in here by the array-builder
-- because we work with small arrays.
type BootstrapForm m = Form m (Array FormElement) String

type IdCounter = Int
newtype FormBuilder m i o = FormBuilder (Compose (State IdCounter) (BootstrapForm m i) o)

derive instance Newtype (FormBuilder m i o) _
derive newtype instance Applicative m => Functor (FormBuilder m i)
derive newtype instance Monad m => Apply (FormBuilder m i)
instance Monad m => Semigroupoid (FormBuilder m) where
  compose (FormBuilder (Compose builder1)) (FormBuilder (Compose builder2)) = formBuilder do
    form1 <- builder1
    form2 <- builder2
    pure $ compose form1 form2

instance Monad m => Category (FormBuilder m) where
  identity = formBuilder $ pure identity

formBuilder :: forall i m o. State IdCounter (BootstrapForm m i o) -> FormBuilder m i o
formBuilder = FormBuilder <<< Compose

unFormBuilder :: forall i m o. FormBuilder m i o -> State IdCounter (BootstrapForm m i o)
unFormBuilder (FormBuilder (Compose builder)) = builder

type FormBuilder' m o = FormBuilder m Query o

evalBuilder :: forall m a. FormBuilder' m a -> BootstrapForm m Query a
evalBuilder (FormBuilder (Compose m)) = evalState m 0

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
  , inline :: Boolean
  , helpText :: Maybe JSX
  , placeholder :: String
  , "type" :: String
  , touched :: Boolean

  -- These make no sens in the context of a text input.
  -- FIXME: We should switch to an easier to compose
  -- representation of optional props (undefined-is-not-a-problem?)
  , max :: Opt Number
  , min :: Opt Number
  , step :: Opt Number
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
  , inline: false
  , missingError: "This field is required"
  , name: Nothing
  , initial: ""
  , placeholder: ""
  , helpText: Nothing
  , "type": "text"
  , touched: false
  , max: NoProblem.undefined
  , min: NoProblem.undefined
  , step: NoProblem.undefined
  }

type TextInputProps m a =
  { validator :: Batteries.Validator m String (Maybe String) a
  | TextInputOptionalPropsRow ()
  }

-- Rendering helper used by fields constructors below.
-- Currently we are following the form layout described here:
--  https://getbootstrap.com/docs/5.0/forms/layout/#horizontal-form
-- TODO: Make it a default prop of the final field
-- constructor.
renderTextInput
  :: { id :: String
     , inline :: Boolean
     , possibleLabel :: Maybe JSX
     , name :: String
     , placeholder :: String
     , type :: String

     , max :: Opt Number
     , min :: Opt Number
     , step :: Opt Number
     }
  -> InputState String
  -> FormElement
renderTextInput props@{ id, inline, possibleLabel, name, placeholder, "type": type_ } { value, errors, onChange, touched } = do
  let
    label = foldMapFlipped possibleLabel \labelJSX ->
      if inline then DOM.label {} [ labelJSX ]
      else DOM.label { className: "col-sm-3 col-form-label-sm" } [ labelJSX ]
    body = do
      let
        { errors: errors', isValid, isInvalid } = fieldValidity touched value errors
        input = Form.textInput
          { id
          , className: if inline then "mb-md-1" else ""
          , name
          , placeholder
          , value
          , onChange: handler targetValue (onChange <<< fromMaybe "")
          , isValid
          , isInvalid
          , "type": type_
          , step: props.step
          , min: props.min
          , max: props.max
          }
      if inline then input
      else DOM.div { className: "col-sm-9" } $
        input
          <> do
            Monoid.guard isInvalid do
              DOM.div { className: "invalid-feedback" }
                [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]
  if inline then DOM.div { className: "col-12 flex-fill" } [ label, body ]
  else DOM.div { className: "row mb-2" } [ label, body ]

textInput
  :: forall m props a
   . Monad m
  => Defaults TextInputOptionalProps { | props } (TextInputProps m a)
  => { | props }
  -> FormBuilder' m a
textInput props = formBuilder do
  { id, name } <- _getIdentifiers props'
  let
    form :: BootstrapForm m Query a
    form = UseForm.input
      (FieldId name)
      props'.initial
      ( Array.singleton <<< renderTextInput
          { id
          , inline: props'.inline
          , possibleLabel: props'.label
          , name
          , placeholder: props'.placeholder
          , "type": props'."type"
          , max: props'.max
          , min: props'.min
          , step: props'.step
          }
      )
      props'.touched
      props'.validator
  pure form
  where
  props' = defaults defaultTextInputProps props

_validator = (Proxy :: Proxy "validator")
_type = (Proxy :: Proxy "type")

_typedTextInput
  :: forall a m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) a | props } (TextInputProps m a)
  => { | props }
  -> String
  -> Batteries.Validator m String (Maybe String) a
  -> FormBuilder' m a
_typedTextInput props type_ validator = do
  textInput props''
  where
  props' = Record.insert _type type_ props

  props'' :: { type :: String, validator :: Batteries.Validator m String (Maybe String) a | props }
  props'' = Record.insert _validator validator props'

type NumberInputOptionalPropsRow r =
  ( max :: Int
  | TextInputOptionalPropsRow r
  )

numberInput
  :: forall m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) Number | props } (TextInputProps m Number)
  => { | props }
  -> FormBuilder' m Number
numberInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator m String String Number
  validator = Batteries.stringifyValidator Batteries.Number.validator

-- optNumberInput
--   :: forall m props
--    . Monad m
--   => Row.Lacks "validator" props
--   => Row.Lacks "type" props
--   => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) (Maybe Number) | props } (TextInputProps m (Maybe Number))
--   => { | props }
--   -> FormBuilder' m Number
-- optNumberInput props = _typedTextInput props "number" validator
--   where
--   validator :: Batteries.Validator m String String Number
--   validator = Batteries.stringifyValidator Batteries.Number.validator

intInput
  :: forall m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) Int | props } (TextInputProps m Int)
  => { | props }
  -> FormBuilder' m Int
intInput props = _typedTextInput props "number" $ requiredV' validator
  where
  validator :: Batteries.Validator m String String Int
  validator = Batteries.stringifyValidator Batteries.Int.validator

decimalInput
  :: forall m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) Decimal | props } (TextInputProps m Decimal)
  => { | props }
  -> FormBuilder' m Decimal
decimalInput props = _typedTextInput props "text" $ requiredV' validator
  where
  formatting = Batteries.Decimal.formatting { decimalSeparator: Just ".", separators: [ " ", "," ] }

  validator :: Batteries.Validator m String String Decimal
  validator = Batteries.stringifyValidator $ Batteries.Decimal.validator formatting

dateInput
  :: forall m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) Date | props } (TextInputProps m Date)
  => { | props }
  -> FormBuilder' m Date
dateInput props = _typedTextInput props "date" validator
  where
  validator :: Batteries.Validator m String (Maybe String) Date
  validator = requiredV' $ liftFnEither \str -> do
    let
      res = Parsing.runParser str parseISODate
    lmap (const [ "Invalid date" ]) $ res

timeInput
  :: forall m props
   . Monad m
  => Row.Lacks "validator" props
  => Row.Lacks "type" props
  => Defaults TextInputOptionalProps { "type" :: String, validator :: Batteries.Validator m String (Maybe String) Time | props } (TextInputProps m Time)
  => { | props }
  -> FormBuilder' m Time
timeInput props = _typedTextInput props "time" $ requiredV' validator
  where
  parseTime :: Parsing.Parser String Time
  parseTime = do
    let
      noteExcept :: forall a. String -> Maybe a -> Parsing.Parser String a
      noteExcept _ (Just a) = pure a
      noteExcept err Nothing = fail err

      parseDigits :: Parsing.Parser String Int
      parseDigits = do
        tens <- parseDigit
        ones <- parseDigit
        pure $ 10 * tens + ones
    (hh :: Hour) <- parseDigits >>= toEnum >>> noteExcept "Invalid hour"
    _ <- colon
    (mm :: Minute) <- parseDigits >>= toEnum >>> noteExcept "Invalid minute"

    pure $ Time hh mm bottom bottom
    where
    colon = Parsing.optional $ Parsing.try $ Parsing.char ':'

  validator :: Batteries.Validator m String String Time
  validator = liftFnEither \str -> do
    let
      res = Parsing.runParser str parseTime
    lmap (const [ "Invalid time" ]) $ res

renderMultiField possibleLabel form = do
  let
    Form (formRecord@{ render }) = form
  Form $ formRecord
    { render = \state -> do
        let
          label =
            DOM.label
              { className: "col-sm-3 col-form-label-sm" } $ fold possibleLabel

          body = DOM.div { className: "col-sm-9" }
            $ DOM.div { className: "row row-cols-lg-auto align-items-center" }
            $ render state
        [ DOM.div { className: "row mb-lg-2 mb-md-1" } [ label, body ] ]
    }

-- TODO: Add help text support and field level error messages handling
dateTimeField :: forall m. Monad m => Maybe JSX -> FormBuilder' m DateTime
dateTimeField possibleLabel = formBuilder do
  di <- unFormBuilder $ dateInput { inline: true }
  ti <- unFormBuilder $ timeInput { inline: true, initial: "00:00" }
  pure $ renderMultiField possibleLabel $ DateTime <$> di <*> ti

type OptTextInputOptionalProps = { | OptTextInputOptionalPropsRow () }

defaultOptTextInputProps :: OptTextInputOptionalProps
defaultOptTextInputProps =
  { label: Nothing
  , id: Nothing
  , initial: ""
  , inline: false
  , name: Nothing
  , placeholder: ""
  , helpText: Nothing
  , "type": "text"
  , touched: false
  , max: NoProblem.undefined
  , min: NoProblem.undefined
  , step: NoProblem.undefined
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
  -> FormBuilder' m (Maybe a)
optTextInput props = formBuilder do
  { id, name } <- _getIdentifiers props'
  let
    render = Array.singleton <<< renderTextInput
      { id
      , inline: props'.inline
      , possibleLabel: props'.label
      , name
      , placeholder: props'.placeholder
      , "type": props'."type"
      , max: props'.max
      , min: props'.min
      , step: props'.step
      }

    form :: BootstrapForm m Query (Maybe a)
    form = UseForm.optInput
      (FieldId name)
      props'.initial
      render
      props'.touched
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
  , inline: false
  , placeholder: ""
  , rows: 3
  , helpText: Nothing
  -- FIXME: We should not use TextInput row as a baseline and we should drop this
  -- from TextArea row.
  , "type": "textarea"
  , touched: false
  , max: NoProblem.undefined
  , min: NoProblem.undefined
  , step: NoProblem.undefined
  }

type TextAreaProps m a =
  { validator :: Batteries.Validator m String (Maybe String) a
  | TextAreaOptionalPropsRow ()
  }

fieldValidity touched value errors = do
  let
    validatedValue = do
      _ /\ vals <- errors
      Array.head vals
    errors' = fold $ map fst errors
    isInvalid = touched && Just value == validatedValue && not (null errors')
    isValid = Just [] == map fst errors
  { errors: errors', isInvalid, isValid }

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
    body = DOM.div { className: "col-sm-9" } do
      let
        { errors: errors', isValid, isInvalid } = fieldValidity touched value errors
      Form.Control.textArea
        { id
        , name
        , placeholder
        , value
        , onChange: handler targetValue (onChange <<< fromMaybe "")
        , rows
        , isValid
        , isInvalid
        }
        <> do
          fold
            [ Monoid.guard isInvalid do
                DOM.div { className: "invalid-feedback" }
                  [ DOOM.ul_ $ map (DOOM.li_ <<< Array.singleton <<< DOOM.text) errors' ]
            , Monoid.guard (isJust helpText && not isInvalid) $
                DOM.div { className: "form-text" }
                  [ fold helpText ]
            ]
  DOM.div { className: "mb-2 row" } [ label, body ]

textArea
  :: forall m props a
   . Monad m
  => Defaults TextAreaOptionalProps { | props } (TextAreaProps m a)
  => { | props }
  -> FormBuilder' m a
textArea props = formBuilder do
  { id, name } <- _getIdentifiers props'
  let
    form :: BootstrapForm m Query a
    form = UseForm.input
      (FieldId name)
      props'.initial
      ( Array.singleton <<< renderTextArea
          { id
          , possibleLabel: props'.label
          , name
          , helpText: props'.helpText
          , placeholder: props'.placeholder
          , rows: props'.rows
          }
      )
      props'.touched
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
  , inline :: Boolean
  , missingError :: String
  , name :: Maybe String
  , initial :: String
  , touched :: Boolean
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
  , inline: false
  , touched: false
  }

type ChoiceFieldProps m a =
  { choices :: ChoiceFieldChoices
  , validator :: Batteries.Validator m String (Maybe String) a
  | ChoiceFieldOptionalPropsRow ()
  }

renderChoiceField
  :: { choices :: ChoiceFieldChoices
     , id :: String
     , inline :: Boolean
     , possibleLabel :: Maybe JSX
     , name :: String
     , possibleHelpText :: Maybe JSX
     }
  -> InputState String
  -> Array FormElement
renderChoiceField { choices, id, inline, possibleHelpText, possibleLabel, name } { value: selectedValue, errors, onChange, touched } = do
  let
    label = foldMapFlipped possibleLabel \labelJSX ->
      if inline then DOM.legend {} [ labelJSX ]
      else DOM.legend { className: "col-sm-3 col-form-label-sm" } [ labelJSX ]

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
              { isValid, isInvalid } = fieldValidity touched value errors

            Form.check
              { disabled
              , id: id <> "-" <> value
              , label: label'
              , isValid
              , isInvalid
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
          className =
            if not inline then "form-check col-sm-9"
            else "form-check"
        DOM.div { className } $ map renderChoice (ArrayAL.toArray choices')

      SelectFieldChoices choices' -> do
        let
          renderOption { disabled, label: label', value } = do
            DOM.option { value, disabled } [ DOOM.text label' ]

          onChangeHandler = handler targetValue \val -> do
            traceM val
            (traverse_ onChange val)

          { isValid, isInvalid } = fieldValidity touched selectedValue errors

          select =
            Bootstrap.Form.select
              { onChange: onChangeHandler
              , className: if inline then "mb-md-1" else ""
              , value: selectedValue
              , id
              , name
              , isValid
              , isInvalid
              } $
              map renderOption (ArrayAL.toArray choices')
        if inline then select
        else DOM.div { className: "col-sm-9" } $
          [ select
          ]
            <> possibleHelpText `flip foldMap` \helpText -> do
              [ DOOM.div
                  { className: "form-text text-muted mt-0 mb-2"
                  , children: [ helpText ]
                  }
              ]

  pure $
    if inline then DOM.div { className: "col-12 flex-fill" } [ label, body ]
    else DOM.div { className: "row mb-2" } [ label, body ]

choiceField
  :: forall m props a
   . Monad m
  => Defaults ChoiceFieldOptionalProps { | props } (ChoiceFieldProps m a)
  => { | props }
  -> FormBuilder' m a
choiceField props = formBuilder do
  { id, name } <- _getIdentifiers props'
  let
    -- input name initial render touched validator = Form
    form = UseForm.input
      (FieldId name)
      props'.initial
      -- props'.missingError
      ( renderChoiceField
          { id
          , inline: props'.inline
          , possibleLabel: props'.label
          , name
          , choices: props'.choices
          , possibleHelpText: props'.helpText
          }
      )
      props'.touched
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
  => Row.Nub (choices :: ChoiceFieldChoices, initial :: String, validator :: Batteries.Validator m String (Maybe String) a | props) props'
  => UseChoiceField a
  -> Maybe (ArrayAL 1 a)
  -> { | props }
  -> FormBuilder' m a
choiceField' useElement possibleArr props = do
  let
    dual :: Polyform.Validator.Dual.Dual m _ _ _
    dual = Batteries.stringifyDual $ UrlEncoded.Duals.enum (Proxy :: Proxy a)
    validator = requiredV' $ Polyform.Dual.parser dual

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

    arr = case possibleArr of
      Nothing -> upFromIncluding bottom :: ArrayAL 1 a
      Just arr' -> arr'
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

