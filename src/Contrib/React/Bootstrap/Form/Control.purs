module Contrib.React.Bootstrap.Form.Control where

import Contrib.React.HTMLAttributes (HTMLAttributes)
import Prim.Row as Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Record as Record
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Value :: Type

value
  :: { array :: Array String -> Value
     , number :: Number -> Value
     , string :: String -> Value
     }
value =
  { "string": unsafeCoerce :: String -> Value
  , "number": unsafeCoerce :: Number -> Value
  , "array": unsafeCoerce :: Array String -> Value
  }

type BaseProps controlValue extraProps =
  HTMLAttributes +
    ( htmlSize :: Int
    , size :: String
    , plaintext :: Boolean
    , readOnly :: Boolean
    , disabled :: Boolean
    , value :: controlValue
    , onChange :: EventHandler
    , isValid :: Boolean
    , isInvalid :: Boolean
    | extraProps
    )

-- | Direct translation from ts to purescript
-- | TODO: Add more handy / safe wrappers.
type Props_control = BaseProps String ("type" :: String)

foreign import _Control :: ReactComponent { | Props_control }

_internalcontrol :: forall attrs attrs_. Row.Union attrs attrs_ Props_control => ReactComponent { | attrs }
_internalcontrol = unsafeCoerce _Control

control
  :: forall attrs attrs_
   . Row.Union attrs attrs_ Props_control
  => Record attrs
  -> JSX
control props = element _internalcontrol props

type Props_inputtext = BaseProps String ()

textInput
  :: forall attrs attrs' attrs_
   . Row.Union attrs' attrs_ Props_control
  => Row.Nub (type :: String | attrs) attrs'
  => Record attrs
  -> JSX
textInput props = element _internalcontrol props'
  where
  props' :: { | attrs' }
  props' = Record.merge { "type": "text" } props