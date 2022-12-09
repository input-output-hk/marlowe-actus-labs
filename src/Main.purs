module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Web.HTML (Window, window)

main :: Effect Unit
main = do
  _ :: Window <- window
  Console.log "hey ho"
  pure unit

-- import Data.Array as Array
-- import Data.Maybe (Maybe(..))
-- import Data.Tuple.Nested ((/\))
-- import Effect (Effect)
-- import Effect.Console as Console
-- import Effect.Exception (throw)
-- import Effect.Timer as Timer
-- import React.Basic.DOM as React.DOM
-- import React.Basic.DOM.Client (createRoot, renderRoot)
-- import React.Basic.Hooks (Component, component)
-- import React.Basic.Hooks as React
-- import Wallet (Wallet)
-- import Wallet as Wallet
-- import Web.DOM (Element)
-- import Web.DOM.NonElementParentNode (getElementById)
-- import Web.HTML (HTMLDocument, Window, window)
-- import Web.HTML.HTMLDocument (toNonElementParentNode)
-- import Web.HTML.Window (document)

-- newtype StartPageState = StartPageState (Array Wallet)

-- data Page = StartPage StartPageState

-- mkActusStartAppPage :: Effect (Maybe Wallet.Cardano) -> Component StartPageState
-- mkActusStartAppPage getCardano =
--   component "ActusStartAppPage" \initialValue -> React.do
--     StartPageState wallets /\ (setStartPageState :: StartPageState -> Effect Unit) <- React.useState' initialValue
--     React.useEffect unit do
--       intervalId <- Timer.setInterval 1_000 do
--         Console.log "long poll!"
--         mCardano :: Maybe Wallet.Cardano <- getCardano
--         case mCardano of
--           Nothing -> setStartPageState $ StartPageState []
--           Just cardano -> setStartPageState <<< StartPageState =<< Wallet.apis cardano
--       pure $ Timer.clearInterval intervalId
--     pure
--       if Array.null wallets then React.DOM.text "No wallets detected!"
--       else React.DOM.text "Wallets detected!"

-- mkActusPrototypeApp :: Effect (Maybe Wallet.Cardano) -> Component Page
-- mkActusPrototypeApp getCardano = do
--   actusStartPage <- mkActusStartAppPage getCardano
--   component "ActusPrototypeApp" \initialValue -> React.do
--     (page :: Page) /\ (_ :: Page -> Effect Unit) <- React.useState' initialValue
--     case page of
--       StartPage startPageState -> pure $ actusStartPage startPageState

-- main :: Effect Unit
-- main = do
--   win :: Window <- window
--   let
--     getCardano :: Effect (Maybe Wallet.Cardano)
--     getCardano = Wallet.cardano win
--   doc :: HTMLDocument <- document win
--   root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
--   case root of
--     Nothing -> throw "Could not find element with id 'app-root'"
--     Just container -> do
--       reactRoot <- createRoot container
--       actusPrototypeApp <- mkActusPrototypeApp getCardano
--       renderRoot reactRoot $ actusPrototypeApp $ StartPage $ StartPageState []
