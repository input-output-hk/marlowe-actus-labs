module Wallet
  ( Address(..)
  , Api
  , Bytes(..)
  , Cbor(..)
  , Coin
  , Hash32(..)
  , Transaction(..)
  , TransactionUnspentOutput
  , Value(..)
  , Wallet
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

import CardanoMultiplatformLib.Transaction (TransactionWitnessSet(..))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import JS.Object (EffectMth0, EffectMth1, EffectMth2, EffectProp, JSObject)
import JS.Object.Generic (mkFFI)
import Promise as Promise
import Promise.Aff (Promise)
import Promise.Aff as Promise
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (Window)

newtype Address = Address String

instance Show Address where
  show (Address s) = "(Address " <> show s <> ")"

data TransactionUnspentOutput

data Coin

data Transaction

data Value

data Hash32

newtype Cbor :: forall k. k -> Type
newtype Cbor a = Cbor String

instance Show (Cbor a) where
  show (Cbor s) = "(Cbor " <> show s <> ")"

newtype Bytes = Bytes String

type Api = JSObject
  ( getNetworkId :: EffectMth0 (Promise Int)
  , getUtxos :: EffectMth0 (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
  , getCollateral :: EffectMth1 (Cbor Coin) (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
  , getBalance :: EffectMth0 (Promise (Cbor Value))
  , getUsedAddresses :: EffectMth0 (Promise (Array Address))
  , getUnusedAddresses :: EffectMth0 (Promise (Array Address))
  , getChangeAddress :: EffectMth0 (Promise Address)
  , getRewardAddresses :: EffectMth0 (Promise (Array Address))
  , signTx :: EffectMth2 (Cbor Transaction) Boolean (Promise (Cbor TransactionWitnessSet))
  , signData :: EffectMth2 Address Bytes (Promise Bytes)
  , submitTx :: EffectMth1 (Cbor Transaction) (Promise (Cbor Hash32))
  )

_Api
  :: { getBalance :: Api -> Effect (Promise (Cbor Value))
     , getChangeAddress :: Api -> Effect (Promise Address)
     , getCollateral :: Api -> Cbor Coin -> Effect (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
     , getNetworkId :: Api -> Effect (Promise Int)
     , getRewardAddresses :: Api -> Effect (Promise (Array Address))
     , getUnusedAddresses :: Api -> Effect (Promise (Array Address))
     , getUsedAddresses :: Api -> Effect (Promise (Array Address))
     , getUtxos :: Api -> Effect (Promise (Nullable (Array (Cbor TransactionUnspentOutput))))
     , signData :: Api -> Address -> Bytes -> Effect (Promise Bytes)
     , signTx :: Api -> Cbor Transaction -> Boolean -> Effect (Promise (Cbor TransactionWitnessSet))
     , submitTx :: Api -> Cbor Transaction -> Effect (Promise (Cbor Hash32))
     }
_Api = mkFFI (Proxy :: Proxy Api)

-- FIXME: newtype this
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

-- | Manually tested and works with Nami.
getBalance :: Api -> Aff (Cbor Value)
getBalance = Promise.toAffE <<< _Api.getBalance

-- | Manually tested and works with Nami.
getChangeAddress :: Api -> Aff Address
getChangeAddress = Promise.toAffE <<< _Api.getChangeAddress

-- | Manually tested and works with Nami.
getCollateral :: Api -> Cbor Coin -> Aff (Array (Cbor TransactionUnspentOutput))
getCollateral api = map (fold <<< Nullable.toMaybe) <<< Promise.toAffE <<< _Api.getCollateral api

-- | Manually tested and works with Nami.
getRewardAddresses :: Api -> Aff (Array Address)
getRewardAddresses = Promise.toAffE <<< _Api.getRewardAddresses

-- | Manually tested and works with Nami.
getUnusedAddresses :: Api -> Aff (Array Address)
getUnusedAddresses = Promise.toAffE <<< _Api.getUnusedAddresses

-- | Manually tested and works with Nami.
getUsedAddresses :: Api -> Aff (Array Address)
getUsedAddresses = Promise.toAffE <<< _Api.getUsedAddresses

-- | Manually tested and works with Nami.
getUtxos :: Api -> Aff (Maybe (Array (Cbor TransactionUnspentOutput)))
getUtxos = map Nullable.toMaybe <<< Promise.toAffE <<< _Api.getUtxos

signData :: Api -> Address -> Bytes -> Aff Bytes
signData api address = Promise.toAffE <<< _Api.signData api address

rejectionToForeign :: Promise.Rejection -> Foreign
rejectionToForeign = unsafeCoerce

toAffEither :: forall a err. (Promise.Rejection -> err) -> Promise.Promise a -> Aff (Either err a)
toAffEither customCoerce p = makeAff \cb ->
  mempty <$
    Promise.thenOrCatch
      (\a -> Promise.resolve <$> cb (Right (Right a)))
      (\e -> Promise.resolve <$> cb (Right (Left (customCoerce e))))
      p

-- FIXME: paluh. Fix error handling by introducing Variant based error
-- representation and using it across the whole API.
toAffEitherE :: forall a err. (Promise.Rejection -> err) -> Effect (Promise a) -> Aff (Either err a)
toAffEitherE coerce f = liftEffect f >>= toAffEither coerce

signTx :: Api -> Cbor Transaction -> Boolean -> Aff (Either Foreign (Cbor TransactionWitnessSet))
signTx api cbor = toAffEitherE rejectionToForeign <<< _Api.signTx api cbor

submitTx :: Api -> Cbor Transaction -> Aff (Cbor Hash32)
submitTx api = Promise.toAffE <<< _Api.submitTx api
