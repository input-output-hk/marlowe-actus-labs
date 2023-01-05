module Utils.React.Basic.Hooks where

import Prelude

import Data.Int as Int
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds(..), Seconds, fromDuration)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Timer (clearTimeout, setTimeout)
import Halogen.Subscription (Emitter, subscribe, unsubscribe) as HS
import React.Basic.Hooks (Hook, Ref, UseEffect, UseRef, UseState, useEffect, useState)
import React.Basic.Hooks (Render) as RB.Hooks
import React.Basic.Hooks as React

type HookApply hooks (newHook :: Type -> Type) = newHook hooks

-- | Applies a new hook to a hook chain, with the innermost hook as the left argument.
-- | This allows hook chains to be written in reverse order, aligning them with the
-- | order they appear when actually used in do-notation.
-- | ```purescript
-- | type UseCustomHook hooks = UseEffect String (UseState Int hooks)
-- | type UseCustomHook' = UseState Int & UseEffect String
-- | ```
infixl 0 type HookApply as &

-- | Move to components?
type UseDebounce a hooks =
  UseState a hooks
    & UseEffect (a /\ Seconds)

useDebounce :: forall a hooks. Eq a => a -> Seconds -> RB.Hooks.Render hooks (UseDebounce a hooks) a
useDebounce value delay = React.do
  let
    delay' = do
      let
        Milliseconds d = fromDuration delay
      Int.floor d
  debouncedValue /\ setDebouncedValue <- useState value

  useEffect (value /\ delay) do
    i <- setTimeout delay' do
      setDebouncedValue (const value)
    pure $ clearTimeout i
  pure debouncedValue

newtype UseEmitter a hooks = UseSignal (UseEffect Unit (UseState a hooks))

derive instance Newtype (UseEmitter a hooks) _

useEmitter :: forall a. a -> HS.Emitter a -> Hook (UseEmitter a) a
useEmitter default emitter =
  React.coerceHook React.do
    value /\ setValue <- React.useState' default
    React.useEffectOnce $ do
      subscription <- HS.subscribe emitter setValue
      pure $ HS.unsubscribe subscription
    pure value

type UseFirstRender hooks =
  UseRef Boolean hooks
    & UseEffect Unit

-- | The hooks doesn't trigger rerender but is wrapped in `Ref`
-- | and can be used in a `useEffect`.
useFirstRender :: Hook UseFirstRender (Ref Boolean)
useFirstRender = React.do
  firstRender <- React.useRef true
  React.useEffectOnce do
    React.writeRef firstRender false
    pure $ pure unit
  pure firstRender
