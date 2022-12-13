module Main
  ( main
  ) where

import Prelude

import Component.ContractList (contractList)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Marlowe.Runtime.Web.Client (fetchContractHeaders)
import Marlowe.Runtime.Web.Types (ResourceLink(..), ServerURL(..))
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM (Element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

serverUrl :: ServerURL
serverUrl = ServerURL "http://localhost:49204/" -- TODO: to config

main :: Effect Unit
main = do
  doc :: HTMLDocument <- document =<< window
  root :: Maybe Element <- getElementById "app-root" $ toNonElementParentNode doc
  case root of
    Nothing -> throw "Could not find element with id 'app-root'"
    Just container -> do
      reactRoot <- createRoot container

      launchAff_ do
        contracts <- fetchContractHeaders serverUrl
        liftEffect $ renderRoot reactRoot (contractList contracts)
