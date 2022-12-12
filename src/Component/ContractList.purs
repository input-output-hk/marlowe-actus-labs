module Component.ContractList where

import Prelude

import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink, ResourceWithLinks)
import React.Basic.DOM as React.DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type ContractList = Array (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState))

contractList :: Component ContractList
contractList = do
  component "ContractList" \initialValue -> React.do
    pure $ React.fragment
      [ React.DOM.div
          { className: "contracts-list"
          , children: [ React.DOM.text "No contracts deployed" ]
          }
      ]
