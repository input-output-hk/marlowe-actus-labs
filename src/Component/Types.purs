module Component.Types where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe)
import Effect (Effect)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader, ResourceWithLinks)
import React.Basic (ReactContext)
import Wallet (Wallet)

type WalletInfo =
  { name :: String
  , icon :: String
  , isEnabled :: Boolean
  , apiVersion :: String
  , wallet :: Wallet
  }

-- We use this monad during creation of the components.
-- This gives us ability to pass down "static" data.
-- which is not changing during the lifetime of the component.
-- `props` can change.
type MkComponentM = ReaderT MkContext Effect

type ContractHeaderResource = ResourceWithLinks ContractHeader (contract :: ContractEndpoint)

type MkContext =
  { walletInfoCtx :: ReactContext (Maybe WalletInfo)
  -- FIXME: use more advanced logger so we use levels and setup app verbosity.
  , logger :: String -> Effect Unit
  -- FIXME: This gonna be replaced by a contract event emitter
  , contracts :: Array ContractHeaderResource
  }


