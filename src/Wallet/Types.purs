module Wallet.Types
  ( Api(..)
  , ApiVersion(..)
  , Cardano
  , Icon(..)
  , Name(..)
  , Wallet(..)
  ) where

import Effect.Aff (Aff)
import Foreign (Foreign)

newtype ApiVersion = ApiVersion String

newtype Icon = Icon String

newtype Name = Name String

{-
TODO implement
-}
newtype Api = Api Foreign

data Wallet = Wallet
  { apiVersion :: ApiVersion
  , enable :: Aff Api
  , icon :: Icon
  , isEnabled :: Aff Boolean
  , name :: Name
  }

foreign import data Cardano :: Type
