module Contrib.React.Bootstrap.Icons
  ( Icon
  , toJSX
  , fileEarmarkPlus
  , wallet2
  )
  where

import Prelude

import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

unsafeIcon :: String -> Icon
unsafeIcon name = Icon $ DOM.span
  { className: "bi-" <> name
  }
  ([] :: Array  JSX)

newtype Icon = Icon JSX

toJSX :: Icon -> JSX
toJSX (Icon jsx) = jsx

wallet2 :: Icon
wallet2 = unsafeIcon "wallet2"

fileEarmarkPlus :: Icon
fileEarmarkPlus = unsafeIcon "file-earmark-plus"
