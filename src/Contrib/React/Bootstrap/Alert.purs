module Contrib.React.Bootstrap.Alert where

import Prelude

import Contrib.React.Bootstrap.Types (Variant)
import Effect (Effect)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type AlertPropsRow = (children :: Array JSX, onClose :: Unit -> Effect Unit, closeLabel :: String, dismissable :: Boolean, show :: Boolean, variant :: Variant)

foreign import _Alert :: ReactComponent { | AlertPropsRow }

alert :: forall r r_. Row.Union r r_ AlertPropsRow => { | r } -> JSX
alert r = element _Alert (unsafeCoerce r)
