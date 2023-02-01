module WalletContext where

import Prelude

import CardanoMultiplatformLib (Bech32, addressObject, allocate, asksLib, bech32FromBytes, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Transaction (transactionOutputObject, transactionUnspentOutput, transactionUnspentOutputObject)
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (for)
import Data.Undefined.NoProblem as NoProblem
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Wallet as Wallet

newtype WalletContext = WalletContext
  { -- , balance :: V1.Assets
    usedAddresses :: Array Bech32
  }

derive instance Newtype WalletContext _
derive newtype instance Show WalletContext
derive newtype instance Eq WalletContext
derive newtype instance Ord WalletContext

walletAddresses :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff (Array Bech32)
walletAddresses cardanoMultiplatformLib wallet = do
  possibleUsedAddresses <- Wallet.getUsedAddresses wallet
  -- let
  --   possibleUsedAddresses = Right []
  -- traceM "Getting UTxOs"
  -- possibleUTxOs <- Wallet.getUtxos wallet
  let
    possibleUTxOs = Right (Just [])

  case possibleUsedAddresses, possibleUTxOs of
    Right addresses, Right (Just utxos) -> do
      liftEffect $ runGarbageCollector cardanoMultiplatformLib do
        _TransactionUnspentOutput <- asksLib _."TransactionUnspentOutput"
        utxoAddresses' <- for utxos \utxo -> do
          let
            utxo' = cborHexToCbor utxo
          unspentTxOutObj <- allocate $ transactionUnspentOutput.from_bytes _TransactionUnspentOutput utxo'
          txOutObj <- allocate $ transactionUnspentOutputObject.output unspentTxOutObj
          addressObj <- allocate $ transactionOutputObject.address txOutObj
          liftEffect $ addressObject.to_bech32 addressObj NoProblem.undefined

        addresses' <- for addresses \addrCborHex -> do
          _Address <- asksLib _."Address"
          bech32FromBytes (cborHexToCbor addrCborHex) NoProblem.undefined

        pure $ Array.nub $ utxoAddresses' <> addresses'
    _, _ -> do
      pure []

walletContext :: CardanoMultiplatformLib.Lib -> Wallet.Api -> Aff WalletContext
walletContext cardanoMultiplatformLib wallet = do
  traceM "Getting wallet context"
  usedAddresses <- walletAddresses cardanoMultiplatformLib wallet
  pure $ WalletContext { usedAddresses }
