module Contrib.React.Basic.Hooks.UseForm where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.FormURLEncoded.Query as Query
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, un)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration (Seconds)
import Data.Validation.Semigroup (V(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Polyform.Batteries as Batteries
import Polyform.Batteries.UrlEncoded as UrlEncoded
import Polyform.Batteries.UrlEncoded as UrleEncoded
import Polyform.Batteries.UrlEncoded.Types.Errors as Errors
import Polyform.Batteries.UrlEncoded.Validators as Validators
import Polyform.Validator (runValidator)
import Polyform.Validator as Validator
import React.Basic.Events (EventHandler, SyntheticEvent, handler_)
import React.Basic.Hooks (type (&), type (/\), Hook, UseEffect, UseMemo, UseState, useEffect, useMemo, useState, useState', (/\))
import React.Basic.Hooks as React
import Safe.Coerce (coerce)
import Type.Row (type (+))
import Utils.React.Basic.Hooks (useDebounce)

type FieldInitialsRow r =
  ( name :: FieldId
  , initial :: Array String
  | r
  )

type FieldInitials = { | FieldInitialsRow () }

type RenderFn err doc = FormState err -> doc

newtype Form m doc err i o = Form
  { fields :: Array FieldInitials
  , validator :: UrlEncoded.Validator m err i o
  , render :: RenderFn err doc
  }

renderForm :: forall doc err i o m. Form m doc err i o -> FormState err -> doc
renderForm (Form { render }) = render

derive instance Applicative m => Functor (Form m doc err i)

instance (Monad m, Semigroup doc) => Apply (Form m doc err i) where
  apply (Form { fields: fields1, validator: validator1, render: render1 }) (Form { fields: fields2, validator: validator2, render: render2 }) =
    Form
      { fields: fields1 <> fields2
      , validator: apply validator1 validator2
      , render: (<>) <$> render1 <*> render2
      }

instance (Monad m, Monoid doc) => Applicative (Form m doc err i) where
  pure a = Form { fields: [], validator: pure a, render: const mempty }

instance (Monad m, Semigroup doc) => Semigroupoid (Form m doc err) where
  compose (Form { fields: fields1, validator: validator1, render: render1 }) (Form { fields: fields2, validator: validator2, render: render2 }) =
    Form
      { fields: fields1 <> fields2
      , validator: compose validator1 validator2
      , render: (<>) <$> render2 <*> render1
      }

instance (Monad m, Monoid doc) => Category (Form m doc err) where
  identity = Form { fields: mempty, validator: identity, render: mempty }

hoistForm :: forall doc err m m' i o. Functor m => (m ~> m') -> Form m doc err i o -> Form m' doc err i o
hoistForm f (Form { fields, validator, render }) =
  Form { fields, validator: Validator.hoist f validator, render }

type InputFieldStateRow err r =
  ( errors :: Maybe (Array err)
  , onChange :: String -> Effect Unit
  , touched :: Boolean
  , value :: String
  | FieldInitialsRow
      + r
  )

type InputState err = { | InputFieldStateRow err () }

type RenderInputFn err doc = InputState err -> doc

toInputState :: forall err. FieldState err -> InputState err
toInputState fieldState =
  { errors: fieldState.errors
  , onChange: fieldState.onChange <<< Array.singleton
  , touched: un Disj fieldState.touched
  , value: fromMaybe "" $ Array.head fieldState.value
  , name: fieldState.name
  , initial: fieldState.initial
  }

input
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> err
  -> RenderInputFn err doc
  -> Batteries.Validator m err String a
  -> Form m doc err Query a
input name initial err render validator = Form
  { fields: [ { name, initial: [ initial ] } ]
  , validator: Validators.required name err $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

optInput
  :: forall a doc err m
   . Monad m
  => Monoid doc
  => FieldId
  -> String
  -> RenderInputFn err doc
  -> Validators.SingleValueFieldValidator m err a
  -> Form m doc err Query (Maybe a)
optInput name initial render validator = Form
  { fields: [ { name, initial: [ initial ] } ]
  , validator: Validators.optional name $ validator
  , render: \state -> do
      let
        doc = Map.lookup name state.fields <#> render <<< toInputState
      fromMaybe mempty doc
  }

multiSelect
  :: forall a doc err m
   . Monad m
  => FieldId
  -> Array String
  -> err
  -> RenderFn err doc
  -> Batteries.Validator m err (NonEmptyArray String) (NonEmptyArray a)
  -> Form m doc err Query (NonEmptyArray a)
multiSelect name initial err render validator = Form
  { fields: [ { name, initial } ]
  , validator: Validators.requiredMulti name err $ validator
  , render
  }

type Props doc err o =
  { onSubmit ::
      { payload :: Query
      , result :: Maybe (V (UrlEncoded.Errors err) o)
      }
      -> Effect Unit
  , spec :: Form Effect doc err Query o
  , validationDebounce :: Seconds
  }

newtype UseForm err o hooks = UseForm
  ( UseState (Set FieldId) hooks
      & UseState (Maybe (V (UrleEncoded.Errors err) o))
      & UseMemo (Array FieldInitials) Query
      & UseState Query
      & UseState Query
      & UseEffect (Query /\ Seconds)
      & UseEffect Query
  )

derive instance Newtype (UseForm o err hooks) _

type FieldStateRow err r =
  ( errors :: Maybe (Array err)
  , onChange :: Array String -> Effect Unit
  , touched :: Disj Boolean
  , value :: Array String
  | FieldInitialsRow
      + r
  )

type FieldState err = { | FieldStateRow err () }

type RenderFieldFn err doc = FieldState err -> doc

type FieldsState err = Map FieldId (FieldState err)

type FormState err =
  { fields :: FieldsState err
  , errors :: Maybe (UrleEncoded.Errors err)
  -- , state :: state
  }

type Result err o =
  { formState :: FormState err
  , onSubmit :: EffectFn1 SyntheticEvent Unit
  , result :: Maybe (V (UrlEncoded.Errors err) o)
  }

useForm :: forall doc err o. Props doc err o -> Hook (UseForm err o) (Result err o)
useForm ({ spec: Form { fields, validator }, onSubmit, validationDebounce }) = React.coerceHook React.do
  touched /\ updateTouched <- useState (mempty :: Set FieldId)
  validationResult /\ setValidationResult <- useState' Nothing

  initialPayload <- useMemo fields \_ -> do
    let
      payload = fields <#> \{ name, initial } -> name /\ initial
    Query.fromFoldable payload

  currPayload /\ updatePayload <- useState initialPayload

  debouncedPayload <- useDebounce currPayload validationDebounce

  useEffect debouncedPayload do
    when (not <<< null $ touched) do
      result <- runValidator validator debouncedPayload
      setValidationResult $ Just result
    pure $ pure unit

  let
    updateField :: FieldId -> Array String -> Effect Unit
    updateField name value = do
      updatePayload $ Query.insert name value
      updateTouched (Set.insert name)

    onSubmit' :: EventHandler
    onSubmit' = handler_ do
      onSubmit { payload: currPayload, result: validationResult }

    fieldsState = Map.fromFoldable $ fields <#> \{ name, initial } -> do
      let
        value = fromMaybe [] $ Query.lookup name currPayload
        fieldErrors = do
          V res <- validationResult
          case res of
            Left errs -> pure $ Errors.lookup (coerce name) errs
            Right _ -> pure []
      (name /\ { name, initial, value, errors: fieldErrors, touched: Disj (name `Set.member` touched), onChange: updateField name })
    formState =
      { fields: fieldsState
      , errors: validationResult >>= case _ of
          V (Left errs) -> Just errs
          V (Right _) -> Nothing
      }
  pure { formState, onSubmit: onSubmit', result: validationResult }

