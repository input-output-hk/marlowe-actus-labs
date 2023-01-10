module CardanoMultiplatformLib.Lib
  ( props
  , Lib
  , Props
  ) where

import CardanoMultiplatformLib.Transaction as Transaction

type Props =
  { "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  }

newtype Lib = Lib
  { "Transaction" :: Transaction.Transaction
  , "TransactionWitnessSet" :: Transaction.TransactionWitnessSet
  , "TransactionBody" :: Transaction.TransactionBody
  }

props :: Lib -> Props
props (Lib r) = r
