module Contrib.React.Hooks where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import Halogen.Subscription (Emitter, subscribe, unsubscribe)
import React.Basic.Hooks (Hook, UseEffect, coerceHook, useEffectOnce)

newtype UseSubscribe :: Type -> Type -> Type
newtype UseSubscribe a hooks = UseAff (UseEffect Unit hooks)

derive instance Newtype (UseSubscribe a hooks) _

useSubscribe :: forall a. Emitter a -> (a -> Effect Unit) -> Hook (UseSubscribe a) Unit
useSubscribe emitter callback = coerceHook React.do
  useEffectOnce do
    subscription <- subscribe emitter callback
    pure $ unsubscribe subscription
