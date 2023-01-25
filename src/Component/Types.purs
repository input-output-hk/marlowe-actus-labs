module Component.Types
  ( MkContextBase(..)
  , MkComponentMBase(..)
  , MkComponentM
  , MessageHub(..)
  , Message(..)
  , MessageId(..)
  , MessageContent(..)
  , WalletInfo(..)
  , module Exports
  )
  where

import Prelude

import CardanoMultiplatformLib as CardanoMultiplatformLib
import Component.Types.ContractInfo (ContractInfo(..), UserCashFlowDirection(..), UserContractRole(..), ActusContractRole(..), CashFlowInfo(..), ActusContractId(..)) as Exports
import Control.Monad.Reader (ReaderT)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsStream)
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
  , contractStream :: ContractWithTransactionsStream
  -- , transactionStream :: ContractTransactionsStream
  , runtime :: Runtime
  , msgHub :: MessageHub
  , aboutMarkdown :: String
  | r
  }

-- We use this monad during creation of the components.
-- This gives us ability to pass down "static" data.
-- which is not changing during the lifetime of the component.
-- `props` can change.
type MkComponentMBase r = ReaderT (MkContextBase r) Effect

type MkComponentM = MkComponentMBase ()

