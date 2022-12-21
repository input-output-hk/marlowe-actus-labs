module Component.Modal where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (fromHomogeneous)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (component, useEffectOnce)
import React.Basic.Hooks as React
import Web.DOM.Document (getElementsByTagName)
import Web.DOM.Document as Document
import Web.DOM.Element (removeAttribute, setAttribute)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type Props =
  { body :: JSX
  , onDismiss :: Effect Unit
  , title :: JSX
  }

-- | TODO:
-- | We could also accept something like parent container on which to render
-- | the modal and display a backdrop.
mkModal :: Effect (Props -> JSX)
mkModal = do
  component "Modal" \{ body, onDismiss, title } -> React.do
    useEffectOnce do
      doc <- (window >>= document) <#> toDocument
      getElementsByTagName "body" doc >>= HTMLCollection.toArray >>= case _ of
        [ root ] -> do
          -- | FIXME: We should not override the existing classes etc.
          setAttribute "style" "overflow: hidden; padding-right: 15px" root
          setAttribute "class" "modal-open" root
          backdrop <- Document.createElement "div" doc
          setAttribute "class" "modal-backdrop fade show" backdrop
          let
            close = do
              log "Close Click"
              removeAttribute "style" root
              removeAttribute "class" root
              Node.removeChild (Element.toNode backdrop) (Element.toNode root)
          Node.appendChild (Element.toNode backdrop) (Element.toNode root)
          pure close
        _ -> pure (pure unit)
    let
      -- Modal covers all the screen really.
      onModalClicked = handler_ onDismiss
      -- If the click started in the modal we don't want to propage it to the
      -- other handler.
      onModalDialogClicked = handler stopPropagation (const $ pure unit)

    pure $
      DOM.div { className: "modal fade show", onClick: onModalClicked, style: css { "display": "block" }, _aria: fromHomogeneous { modal: "true" }, role: "dialog" }
        [ DOM.div { className: "modal-dialog", onClick: onModalDialogClicked }
            [ DOM.div { className: "modal-content" }
                [ DOM.div { className: "modal-header" }
                    [ DOM.h5 { className: "modal-title" }
                        [ (title :: JSX) ]
                    , DOM.button { className: "btn-close", _data: fromHomogeneous { "bs-dismiss": "modal" }, onClick: handler_ onDismiss, type: "button", _aria: fromHomogeneous { label: "Close" } }
                        ([] :: Array JSX)
                    ]
                , DOM.div { className: "modal-body" }
                    [ (body :: JSX) ]
                ]
            ]
        ]
