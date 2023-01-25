module Component.Types where

import Prelude

import Actus.Domain as Actus
import CardanoMultiplatformLib as CardanoMultiplatformLib
import Contrib.Data.BigInt.PositiveBigInt (PositiveBigInt)
import Control.Monad.Reader (ReaderT)
import Data.BigInt.Argonaut (BigInt(..))
import Data.Lazy (Lazy)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Web (Runtime)
import Marlowe.Runtime.Web.Streaming (ContractWithTransactionsStream)
import Marlowe.Runtime.Web.Types as Runtime
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

data UserContractRole
  = ContractParty | ContractCounterParty | BothParties

data ActusContractRole = ActusParty | ActusCounterParty

-- Cash flow direction in the context of the wallet.
data UserCashFlowDirection
  = IncomingFlow
  | OutgoingFlow
  | InternalFlow

newtype CashFlowInfo = CashFlowInfo
  { -- Author of the transaction - either party or counter party.
    cashFlow :: Actus.CashFlow V1.Value V1.Party
  , sender :: ActusContractRole
  -- From the current wallet perspective (if relatd to the user).
  , userCashFlowDirection :: Maybe (UserCashFlowDirection  /\ PositiveBigInt)
  , token :: V1.Token
  -- Value from ACTUS perspective.
  , value :: BigInt
  }

newtype ContractInfo = ContractInfo
  { cashFlowInfo :: Lazy (Array CashFlowInfo)
  , counterParty :: V1.Party
  , contractId :: Runtime.ContractId
  , contractTerms :: Actus.ContractTerms
  , userContractRole :: Maybe UserContractRole
  , party :: V1.Party
  , endpoints ::
    { contract :: Runtime.ContractEndpoint
    , transactions :: Maybe Runtime.TransactionsEndpoint
    }
  -- Use this only for debugging - all domain specific data
  -- should be precomputed and exposed as separated fields.
  , _runtime ::
    { contractHeader :: Runtime.ContractHeader
    , transactions :: Array Runtime.Tx
    }
  }


