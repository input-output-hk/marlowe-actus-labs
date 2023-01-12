module CardanoMultiplatformLib.Lib
  ( props
  , Lib
  , Props
  ) where

import Prelude

import CardanoMultiplatformLib.Address as Address
import CardanoMultiplatformLib.Transaction as Transaction

type Props =
  { "Address" :: Address.Address
  , "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  }

newtype Lib = Lib
  { "Address" :: Address.Address
  , "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  }

props :: Lib -> Props
props (Lib r) = r


