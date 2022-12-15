module Wallet
  ( Address(..)
  , Bytes(..)
  , Cbor(..)
  , apiVersion
  , cardano
  , enable
  , getBalance
  , getChangeAddress
  , getCollateral
  , getNetworkId
  , getRewardAddresses
  , getUnusedAddresses
  , getUsedAddresses
  , getUtxos
  , icon
  , isEnabled
  , name
  , nami
  , signData
  , signTx
  , submitTx
  , yoroi
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectProp, JSObject)
import JS.Object.Generic (mkFFI)
import Promise.Aff (Promise)
import Promise.Aff as Promise
import Type.Prelude (Proxy(..))
import Web.HTML (Window)

newtype Address = Address String

instance Show Address where
  show (Address s) = show s

newtype Cbor = Cbor String

instance Show Cbor where
  show (Cbor s) = show s

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

-- | Manually tested and works with Nami (after a delay)
-- |
-- | The Nami and Yoroi browser extensions injects themselves into the
-- | running website with
-- | ```js
-- | window.cardano = { ...window.cardano, nami = stuff }
-- | ```
cardano :: Window -> Effect (Maybe Cardano)
cardano w = do
  eProp <- runExceptT $ Foreign.Index.readProp "cardano" $ Foreign.unsafeToForeign w
  case eProp of
    Left e -> throw $ show e
    Right prop
      | Foreign.isUndefined prop -> pure Nothing
      | otherwise -> pure $ Just $ Foreign.unsafeFromForeign prop

-- | Manually tested and works with Nami.
-- |
-- | Remember that the Nami browser extension injects itself with
-- | ```js
-- | window.cardano = { ...window.cardano, nami = stuff }
-- | ```
-- | after a delay so if you want to wait for it with an artificial delay,
-- | you have to preceed the delay before invoking `cardano` rather than
-- | this procedure.
nami :: Cardano -> Effect (Maybe Wallet)
nami = map Nullable.toMaybe <<< _Cardano.nami

-- | Not yet manually tested.
yoroi :: Cardano -> Effect (Maybe Wallet)
yoroi = map Nullable.toMaybe <<< _Cardano.yoroi

-- | Manually tested and works with Nami.
apiVersion :: Wallet -> Effect String
apiVersion = _Wallet.apiVersion

-- | Manually tested and works with Nami.
enable :: Wallet -> Aff Api
enable = Promise.toAffE <<< _Wallet.enable

-- | Manually tested and works with Nami.
icon :: Wallet -> Effect String
icon = _Wallet.icon

-- | Manually tested and works with Nami.
isEnabled :: Wallet -> Aff Boolean
isEnabled = Promise.toAffE <<< _Wallet.isEnabled

-- | Manually tested and works with Nami.
name :: Wallet -> Effect String
name = _Wallet.name

-- | Manually tested and works with Nami.
getNetworkId :: Api -> Aff Int
getNetworkId = Promise.toAffE <<< _Api.getNetworkId

-- | Manually tested and does not work with Nami.
getBalance :: Api -> Aff Number
getBalance = Promise.toAffE <<< _Api.getBalance

getChangeAddress :: Api -> Aff Address
getChangeAddress = Promise.toAffE <<< _Api.getChangeAddress

getCollateral :: Api -> Cbor -> Aff (Array Cbor)
getCollateral api = Promise.toAffE <<< _Api.getCollateral api

getRewardAddresses :: Api -> Aff (Array Address)
getRewardAddresses = Promise.toAffE <<< _Api.getRewardAddresses

getUnusedAddresses :: Api -> Aff (Array Address)
getUnusedAddresses = Promise.toAffE <<< _Api.getUnusedAddresses

getUsedAddresses :: Api -> Aff (Array Address)
getUsedAddresses = Promise.toAffE <<< _Api.getUsedAddresses

getUtxos :: Api -> Aff (Array Cbor)
getUtxos = Promise.toAffE <<< _Api.getUtxos

signData :: Api -> Address -> Bytes -> Aff Bytes
signData api address = Promise.toAffE <<< _Api.signData api address

signTx :: Api -> Cbor -> Boolean -> Aff Cbor
signTx api cbor = Promise.toAffE <<< _Api.signTx api cbor

submitTx :: Api -> Cbor -> Aff Cbor
submitTx api = Promise.toAffE <<< _Api.submitTx api
