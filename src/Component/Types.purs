module Component.Types where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Control.Monad.Reader (ReaderT)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader, ResourceWithLinks)
import React.Basic (ReactContext)
import Wallet as Wallet

newtype WalletInfo wallet = WalletInfo
  { name :: String
  , icon :: String
  , isEnabled :: Boolean
  , apiVersion :: String
  , wallet :: wallet
  }

derive instance Newtype (WalletInfo wallet) _

data ContractEvent
  = Addition ContractHeader
  | Deletion ContractHeader
  | Update { old :: ContractHeader, new :: ContractHeader }

-- We use this monad during creation of the components.
-- This gives us ability to pass down "static" data.
-- which is not changing during the lifetime of the component.
-- `props` can change.
type MkComponentM = ReaderT MkContext Effect

type ContractHeaderResource = ResourceWithLinks ContractHeader (contract :: ContractEndpoint)

type MkContext =
  { cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , walletInfoCtx :: ReactContext (Maybe (WalletInfo Wallet.Api))
  -- FIXME: use more advanced logger so we use levels and setup app verbosity.
  , logger :: String -> Effect Unit
  , contractEmitter :: Subscription.Emitter ContractEvent
  , runtime :: Runtime
  }

