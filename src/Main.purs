module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as React.DOM
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Wallet (Wallet)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

newtype StartPageState = StartPageState (Array Wallet)

data Page = StartPage StartPageState

actusStartPage :: StartPageState -> React.JSX
actusStartPage (StartPageState wallets)
  | Array.null wallets = React.DOM.text "No wallets detected"
  | otherwise = React.DOM.text "Wallets detected!"

mkActusPrototype :: Component Page
mkActusPrototype = do
  component "Counter" \initialValue -> React.do
    (page :: Page) /\ (_ :: Page -> Effect Unit) <- React.useState' initialValue
    case page of
      StartPage startPageState -> pure $ actusStartPage startPageState

main :: Effect Unit
main = do
  w :: Window <- window
  doc :: HTMLDocument <- document w
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      actusPrototype <- mkActusPrototype
      renderRoot reactRoot $ actusPrototype $ StartPage $ StartPageState []
