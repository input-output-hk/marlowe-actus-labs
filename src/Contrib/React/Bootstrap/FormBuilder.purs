module Contrib.React.Bootstrap.FormBuilder where

import Prelude

import Contrib.React.Basic.Hooks.UseForm (Form, InputState)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.Form as Form
import Contrib.React.Bootstrap.Form.Control as Form.Control
import Control.Monad.State (State, evalState, get, put)
import ConvertableOptions (class Defaults, defaults)
import Data.Array as Array
import Data.Foldable (foldMap, null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Functor.Compose (Compose(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Polyform.Batteries as Batteries
import React.Basic (JSX)
import React.Basic.DOM as RDOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

type BootstrapForm m = Form m (Array JSX) String

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
  }

type TextInputProps m a =
  { validator :: Batteries.Validator m String String a
  | TextInputOptionalPropsRow ()
  }

renderTextInput
  :: String
  -> Maybe JSX
  -> String
  -> InputState String
  -> Array JSX
renderTextInput id possibleLabel name { value, errors, onChange, touched } = do
  let
    label = possibleLabel `flip foldMap` \labelJsx ->
      [ RDOM.label
          { htmlFor: id
          , children: [ labelJsx ]
          }
      ]
  label <>
    [ Form.textInput
        { id
        , name
        , value
        , onChange: handler targetValue (onChange <<< fromMaybe "")
        , isValid: maybe false null errors
        , isInvalid: touched && maybe false (not <<< null) errors
        }
    ]

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
      (renderTextInput id props'.label name)
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
    render { value, errors, onChange, touched } = Array.singleton $ Form.textInput
      { id
      , name
      , value
      , onChange: handler targetValue (onChange <<< fromMaybe "")
      , isValid: maybe false null errors
      , isInvalid: touched && maybe false (not <<< null) errors
      }

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
  }

type TextAreaProps m a =
  { validator :: Batteries.Validator m String String a
  | TextAreaOptionalPropsRow ()
  }

renderTextArea
  :: String
  -> Maybe JSX
  -> String
  -> Int
  -> InputState String
  -> Array JSX
renderTextArea id possibleLabel name rows { value, errors, onChange, touched } = do
  let
    label = possibleLabel `flip foldMap` \labelJsx ->
      [ RDOM.label
          { htmlFor: id
          , children: [ labelJsx ]
          }
      ]
  label <>
    [ Form.Control.textArea
        { id
        , name
        , value
        , onChange: handler targetValue (onChange <<< fromMaybe "")
        , rows
        , isValid: maybe false null errors
        , isInvalid: touched && maybe false (not <<< null) errors
        }
    ]

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
      (renderTextArea id props'.label name props'.rows)
      props'.validator
  pure form
  where
  props' = defaults defaultTextAreaProps props

