module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (throw)
import Effect.Timer as Timer
import Foreign.Object (Object)
import Foreign.Object as Object
import React.Basic.DOM as React.DOM
import React.Basic.DOM.Client (createRoot, renderRoot)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Wallet (Wallet(..))
import Wallet as Wallet
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

newtype StartPageState = StartPageState (Array Wallet)

data Page = StartPage StartPageState

mkActusStartAppPage :: Effect (Maybe (Object Wallet)) -> Component StartPageState
mkActusStartAppPage getWallets =
  component "ActusStartAppPage" \initialValue -> React.do
    StartPageState wallets /\ (setStartPageState :: StartPageState -> Effect Unit) <- React.useState' initialValue
    React.useEffect unit do
      intervalId <- Timer.setInterval 1_000 do
        Console.log "long poll!"
        setStartPageState <<< StartPageState <<< maybe [] Object.values =<< getWallets
      pure $ Timer.clearInterval intervalId
    pure
      if Array.null wallets then React.DOM.text "No wallets detected!"
      else React.fragment do
        Wallet { name: Wallet.Name name } <- wallets
        [ React.DOM.text name
        , React.DOM.br {}
        ]

mkActusPrototypeApp :: Effect (Maybe (Object Wallet)) -> Component Page
mkActusPrototypeApp getWallets = do
  actusStartPage <- mkActusStartAppPage getWallets
  component "ActusPrototypeApp" \initialValue -> React.do
    (page :: Page) /\ (_ :: Page -> Effect Unit) <- React.useState' initialValue
    case page of
      StartPage startPageState -> pure $ actusStartPage startPageState

main :: Effect Unit
main = do
  win :: Window <- window
  doc :: HTMLDocument <- document win
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container
      actusPrototypeApp <- mkActusPrototypeApp (Wallet.getWallets win)
      renderRoot reactRoot $ actusPrototypeApp $ StartPage $ StartPageState []
