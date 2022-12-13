module Wallet where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (ForeignError)
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectProp, JSObject)
import JS.Object.Generic (mkFFI)
import Type.Prelude (Proxy(..))
import Web.HTML (Window)
import Web.Promise (Promise)

newtype Address = Address String

newtype Cbor = Cbor String

newtype Bytes = Bytes String

type Api = JSObject
  ( getNetworkId :: EffectMth0 (Promise Int)
  , getUtxos :: EffectMth0 (Promise (Array Cbor))
  , getCollateral :: EffectMth1 Cbor (Promise (Array Cbor))
  , getBalance :: EffectMth0 (Promise Number)
  , getUsedAddresses :: EffectMth0 (Promise (Array Address))
  , getUnusedAddresses :: EffectMth0 (Promise (Array Address))
  , getChangeAddress :: EffectMth0 (Promise Address)
  , getRewardAddresses :: EffectMth0 (Promise (Array Address))
  , signTx :: EffectMth2 Cbor Boolean (Promise Cbor)
  , signData :: EffectMth2 Address Bytes (Promise Bytes)
  , submitTx :: EffectMth1 Cbor (Promise Cbor)
  )

_Api
  :: { getBalance :: Api -> Effect (Promise Number)
     , getChangeAddress :: Api -> Effect (Promise Address)
     , getCollateral :: Api -> Cbor -> Effect (Promise (Array Cbor))
     , getNetworkId :: Api -> Effect (Promise Int)
     , getRewardAddresses :: Api -> Effect (Promise (Array Address))
     , getUnusedAddresses :: Api -> Effect (Promise (Array Address))
     , getUsedAddresses :: Api -> Effect (Promise (Array Address))
     , getUtxos :: Api -> Effect (Promise (Array Cbor))
     , signData :: Api -> Address -> Bytes -> Effect (Promise Bytes)
     , signTx :: Api -> Cbor -> Boolean -> Effect (Promise Cbor)
     , submitTx :: Api -> Cbor -> Effect (Promise Cbor)
     }
_Api = mkFFI (Proxy :: Proxy Api)

type Wallet = JSObject
  ( enable :: EffectMth0 (Promise Api)
  , isEnabled :: EffectMth0 (Promise Boolean)
  , apiVersion :: EffectProp String
  , name :: EffectProp String
  , icon :: EffectProp String
  )

_Wallet
  :: { apiVersion :: Wallet -> Effect String
     , enable :: Wallet -> Effect (Promise Api)
     , icon :: Wallet -> Effect String
     , isEnabled :: Wallet -> Effect (Promise Boolean)
     , name :: Wallet -> Effect String
     }
_Wallet = mkFFI (Proxy :: Proxy Wallet)

type Cardano = JSObject
  ( nami :: EffectProp (Nullable Wallet)
  , yoroi :: EffectProp (Nullable Wallet)
  )

_Cardano
  :: { nami :: Cardano -> Effect (Nullable Wallet)
     , yoroi :: Cardano -> Effect (Nullable Wallet)
     }
_Cardano = mkFFI (Proxy :: Proxy Cardano)

throwForeignReadError :: forall a. ExceptT (NonEmptyList ForeignError) Effect a -> Effect a
throwForeignReadError = either (throw <<< show) pure <=< runExceptT

cardano :: Window -> Effect (Maybe Cardano)
cardano w = do
  prop <- throwForeignReadError $ Foreign.Index.readProp "cardano" $ Foreign.unsafeToForeign w
  pure $
    if Foreign.isUndefined prop then Nothing
    else Just $ Foreign.unsafeFromForeign prop
