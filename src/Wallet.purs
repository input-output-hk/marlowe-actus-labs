module Wallet
  ( Address(..)
  , Api
  , Cardano
  , Cbor
  , Paginate
  , Wallet
  , apiVersion
  , cardano
  , enable
  , icon
  , isEnabled
  , name
  , nami
  , yoroi
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise as Promise
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Foreign (ForeignError)
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import Web.HTML (Window)

foreign import data Cardano :: Type

foreign import data Wallet :: Type

foreign import data Api :: Type

foreign import data Paginate :: Type

newtype Address = Address String

newtype Cbor = Cbor String

throwForeignReadError :: forall a. ExceptT (NonEmptyList ForeignError) Effect a -> Effect a
throwForeignReadError = either (throw <<< show) pure <=< runExceptT

cardano :: Window -> Effect (Maybe Cardano)
cardano w = do
  prop <- throwForeignReadError $ Foreign.Index.readProp "cardano" $ Foreign.unsafeToForeign w
  pure $
    if Foreign.isUndefined prop then Nothing
    else Just $ Foreign.unsafeFromForeign prop

nami :: Cardano -> Effect (Maybe Wallet)
nami c = do
  prop <- throwForeignReadError $ Foreign.Index.readProp "nami" $ Foreign.unsafeToForeign c
  pure $
    if Foreign.isUndefined prop then Nothing
    else Just $ Foreign.unsafeFromForeign prop

yoroi :: Cardano -> Effect (Maybe Wallet)
yoroi c = do
  prop <- throwForeignReadError $ Foreign.Index.readProp "yoroi" $ Foreign.unsafeToForeign c
  pure $
    if Foreign.isUndefined prop then Nothing
    else Just $ Foreign.unsafeFromForeign prop

apiVersion :: Wallet -> Effect String
apiVersion =
  throwForeignReadError
    <<< (Foreign.readString <=< Foreign.Index.readProp "apiVersion")
    <<< Foreign.unsafeToForeign

enable :: Wallet -> Effect (Aff Api)
enable =
  throwForeignReadError
    <<< map Promise.toAffE
    <<< (Foreign.unsafeReadTagged "Function" <=< Foreign.Index.readProp "enable")
    <<< Foreign.unsafeToForeign

icon :: Wallet -> Effect String
icon =
  throwForeignReadError
    <<< (Foreign.readString <=< Foreign.Index.readProp "icon")
    <<< Foreign.unsafeToForeign

isEnabled :: Wallet -> Effect (Aff Boolean)
isEnabled =
  throwForeignReadError
    <<< map Promise.toAffE
    <<< (Foreign.unsafeReadTagged "Function" <=< Foreign.Index.readProp "isEnabled")
    <<< Foreign.unsafeToForeign

name :: Wallet -> Effect String
name =
  throwForeignReadError
    <<< (Foreign.readString <=< Foreign.Index.readProp "name")
    <<< Foreign.unsafeToForeign

getNetworkId :: Api -> Effect (Aff Int)
getNetworkId = undefined

getUtxos :: Api -> Effect (Aff (Array Cbor))
getUtxos = undefined

getCollateral :: Api -> Effect (Cbor -> Aff (Array Cbor))
getCollateral = undefined

getBalance :: Api -> Effect (Aff Number)
getBalance = undefined

getUsedAddresses :: Api -> Effect (Aff (Array Address))
getUsedAddresses = undefined

getUnusedAddresses :: Api -> Effect (Aff (Array Address))
getUnusedAddresses = undefined

getChangeAddress :: Api -> Effect (Aff Address)
getChangeAddress = undefined

getRewardAddresses :: Api -> Effect (Aff (Array Address))
getRewardAddresses = undefined

signTx :: Api -> Effect (Cbor -> Aff (Array Address))
signTx = undefined
