module Component.Types where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Control.Monad.Reader (ReaderT)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Types (ContractEndpoint, ContractHeader, GetContractResponse, ResourceWithLinks, TxOutRef, GetContractsResponse)
import React.Basic (JSX, ReactContext)
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
  = Addition GetContractsResponse
  | Deletion GetContractsResponse
  | Update { old :: GetContractsResponse, new :: GetContractsResponse }

type ContractHeaderResource = ResourceWithLinks ContractHeader (contract :: ContractEndpoint)

data MessageContent
  = Info JSX
  | Success JSX
  | Warning JSX
  | Error JSX

type MessageId = Int

type Message =
  { id :: MessageId
  , msg :: MessageContent
  }

newtype MessageHub = MessageHub
  { add :: MessageContent -> Effect Unit
  , remove :: MessageId -> Effect Unit
  , ctx :: ReactContext (List Message)
  }

type MkContextBase r =
  { cardanoMultiplatformLib :: CardanoMultiplatformLib.Lib
  , walletInfoCtx :: ReactContext (Maybe (WalletInfo Wallet.Api))
  -- FIXME: use more advanced logger so we use levels and setup app verbosity.
  , logger :: String -> Effect Unit
  , contractEmitter :: Subscription.Emitter ContractEvent
  , getContracts :: Effect (Map TxOutRef GetContractsResponse)
  , runtime :: Runtime
  , msgHub :: MessageHub
  | r
  }

-- We use this monad during creation of the components.
-- This gives us ability to pass down "static" data.
-- which is not changing during the lifetime of the component.
-- `props` can change.
type MkComponentMBase r = ReaderT (MkContextBase r) Effect

type MkComponentM = MkComponentMBase ()

