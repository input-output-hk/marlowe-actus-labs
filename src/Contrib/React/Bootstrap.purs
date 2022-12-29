module Contrib.React.Bootstrap where

import Prelude

import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Color :: Type

foreign import data Variant :: Type

color :: { danger :: Color, dark :: Color, info :: Color, light :: Color, muted :: Color, primary :: Color, secondary :: Color, warning :: Color, white :: Color }
color =
  { primary: unsafeCoerce "primary" :: Color
  , secondary: unsafeCoerce "secondary" :: Color
  , danger: unsafeCoerce "danger" :: Color
  , warning: unsafeCoerce "warning" :: Color
  , info: unsafeCoerce "info" :: Color
  , dark: unsafeCoerce "dark" :: Color
  , light: unsafeCoerce "light" :: Color
  , white: unsafeCoerce "white" :: Color
  , muted: unsafeCoerce "muted" :: Color
  }

-- export type Variant = 'primary' | 'secondary' | 'success' | 'danger' | 'warning' | 'info' | 'dark' | 'light' | string;

variant
  :: { danger :: Variant
     , dark :: Variant
     , info :: Variant
     , light :: Variant
     , primary :: Variant
     , secondary :: Variant
     , success :: Variant
     , warning :: Variant
     }
variant =
  { primary: unsafeCoerce "primary" :: Variant
  , secondary: unsafeCoerce "secondary" :: Variant
  , success: unsafeCoerce "success" :: Variant
  , danger: unsafeCoerce "danger" :: Variant
  , warning: unsafeCoerce "warning" :: Variant
  , info: unsafeCoerce "info" :: Variant
  , dark: unsafeCoerce "dark" :: Variant
  , light: unsafeCoerce "light" :: Variant
  }

type AlertPropsRow = (children :: Array JSX, onClose :: Unit -> Effect Unit, closeLabel :: String, dissmisable :: Boolean, show :: Boolean, variant :: Variant)

foreign import _Alert :: ReactComponent { | AlertPropsRow }

alert :: forall r r_. Row.Union r r_ AlertPropsRow => { | r } -> JSX
alert r = element _Alert (unsafeCoerce r)
