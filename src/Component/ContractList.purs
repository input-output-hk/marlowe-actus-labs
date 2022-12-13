module Component.ContractList where

import Prelude

import Data.Array (filter)
import Data.Map (empty)
import Data.Newtype (unwrap)
import Marlowe.Runtime.Web.Types (ContractHeader(..), ContractState, ResourceLink, ResourceWithLinks)
import React.Basic.DOM as React.DOM
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React


contractList :: Array (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState)) -> JSX
contractList =
  React.fragment
    <<< map
      ( \{ links } ->
          React.DOM.div
            { className: "contracts-list"
            , children: [ React.DOM.text $ unwrap links.contract ]
            }
      )
    <<< filter
      ( \{ resource } ->
          let
            ContractHeader header = resource
          in
            header.metadata == empty -- TODO: find ACTUS contract in Metadata
      )
