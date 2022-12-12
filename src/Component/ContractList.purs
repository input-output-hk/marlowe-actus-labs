module Component.ContractList where

import Prelude

import Data.Newtype (unwrap)
import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink, ResourceWithLinks)
import React.Basic.DOM as React.DOM
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React

type ContractList = Array (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState))

contractList :: ContractList -> JSX
contractList =
  React.fragment <<< map \{ links } ->
    React.DOM.div
      { className: "contracts-list"
      , children: [ React.DOM.text $ unwrap links.contract ]
      }
