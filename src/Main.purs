module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as React.DOM
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Events as React.Events
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkCounter :: Component Int
mkCounter = do
  component "Counter" \initialValue -> React.do
    counter /\ setCounter <- React.useState initialValue
    pure $ React.fragment
      [ React.DOM.text $ "Counter: " <> show counter
      , React.DOM.br {}
      , React.DOM.button
          { onClick: React.Events.handler_ $ setCounter (_ + 1)
          , children: [ React.DOM.text "Increment" ]
          }
      , React.DOM.button
          { onClick: React.Events.handler_ $ setCounter (_ - 1)
          , children: [ React.DOM.text "Decrement" ]
          }
      ]

main :: Effect Unit
main = do
  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      counter <- mkCounter
      renderRoot reactRoot (counter 0)
