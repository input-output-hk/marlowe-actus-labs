module Component.Widgets where

import Prelude

import Contrib.React.Bootstrap.Icons (Icon)
import Contrib.React.Bootstrap.Icons as Icons
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as DOOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (handler)

spinner :: Maybe JSX -> JSX
spinner possibleBody = DOM.div
  { className: "spinner-border"
  , role: "status"
  }
  [ DOM.span { className: "visually-hidden" }
    [ fromMaybe (DOOM.text "Loading...") possibleBody ]
  ]

link :: JSX -> JSX
link label = DOM.button
  { className: "btn btn-link text-decoration-none text-decoration-underline-hover text-reset"
  , type: "button"
  }
  [ label ]

linkWithIcon :: Icon -> JSX -> String -> Effect Unit -> JSX
linkWithIcon icon label className onClick = DOM.button
  { className: "btn btn-link text-decoration-none text-decoration-underline-hover text-reset " <> className
  , onClick: handler preventDefault (const $ onClick)
  , type: "button"
  }
  [ Icons.toJSX icon
  , DOOM.text " "
  , label
  ]

linkButtonWithIcon :: Icon -> JSX -> String -> Effect Unit -> JSX
linkButtonWithIcon icon label className onClick = DOM.button
  { className: "btn btn-link text-decoration-none text-reset border border-1 bg-light-hover " <> className
  , onClick: handler preventDefault (const $ onClick)
  , type: "button"
  }
  [ Icons.toJSX icon
  , DOOM.text " "
  , label
  ]

