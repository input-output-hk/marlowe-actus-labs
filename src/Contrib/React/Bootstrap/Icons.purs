module Contrib.React.Bootstrap.Icons
  ( Icon
  , toJSX
  , cashStack
  , infoSquare
  , fileEarmarkPlus
  , wallet2
  ) where

import Prelude

import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (JSX)

unsafeIcon :: String -> Icon
unsafeIcon name = Icon $ DOM.span
  { className: "bi-" <> name
  }
  ([] :: Array JSX)

newtype Icon = Icon JSX

toJSX :: Icon -> JSX
toJSX (Icon jsx) = jsx

cashStack :: Icon
cashStack = unsafeIcon "cash-stack"

fileEarmarkPlus :: Icon
fileEarmarkPlus = unsafeIcon "file-earmark-plus"

infoSquare :: Icon
infoSquare = unsafeIcon "info-square"

wallet2 :: Icon
wallet2 = unsafeIcon "wallet2"

