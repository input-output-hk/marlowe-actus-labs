module Wallet
  ( Api(..)
  , ApiVersion(..)
  , Icon(..)
  , Name(..)
  , Wallet(..)
  , getWallets
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Web.HTML (Window)
import Web.Promise (Promise)

newtype ApiVersion = ApiVersion String

newtype Icon = Icon String

newtype Name = Name String

{-
TODO implement
-}
newtype Api = Api Foreign

newtype Wallet = Wallet
  { apiVersion :: ApiVersion
  , enable :: Effect (Promise Api)
  , icon :: Icon
  , isEnabled :: Effect (Promise Boolean)
  , name :: Name
  }

foreign import data Cardano :: Type

foreign import getWallets_ :: Window -> Effect (Nullable (Object Wallet))

getWallets :: Window -> Effect (Maybe (Object Wallet))
getWallets = map Nullable.toMaybe <<< getWallets_
