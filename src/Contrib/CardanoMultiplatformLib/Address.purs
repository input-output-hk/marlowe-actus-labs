module CardanoMultiplatformLib.Address where

import Prelude

-- TODO: We shifted to a different Lib representation
--      and this should be not used anymore here. We should remove it.
--      Please check Trasnaction module for an exmple.
import CardanoMultiplatformLib.Lib (Lib)
import Contrib.Effect as Effect
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import HexString as HexString

foreign import data Address :: Type

-- | We allocate the `Address` in the memory. If we don't `free` it up then
-- | we gonna leak memory.
foreign import fromBytesImpl :: EffectFn2 Lib Uint8Array Address

fromBytes :: Lib -> Uint8Array -> Effect Address
fromBytes lib bytes = runEffectFn2 fromBytesImpl lib bytes

foreign import fromBech32Impl :: EffectFn2 Lib String Address

fromBech32 :: Lib -> Bech32 -> Effect Address
fromBech32 lib (Bech32 bech32) = runEffectFn2 fromBech32Impl lib bech32

newtype Bech32 = Bech32 String

bech32ToString :: Bech32 -> String
bech32ToString (Bech32 str) = str

foreign import toBech32Impl :: EffectFn1 Address Bech32

toBech32 :: Address -> Effect Bech32
toBech32 = runEffectFn1 toBech32Impl

foreign import toJsonImpl :: EffectFn1 Address Json

toJson :: Address -> Effect Json
toJson = runEffectFn1 toJsonImpl

foreign import freeImpl :: EffectFn1 Address Unit

free :: Address -> Effect Unit
free = runEffectFn1 freeImpl

bech32FromBytes :: Lib -> Uint8Array -> Effect (Maybe Bech32)
bech32FromBytes lib bytes = do
  let
    go = Effect.bracket (fromBytes lib bytes) free \addr ->
      toBech32 addr
  (Just <$> go) `catchError` (const $ pure Nothing)

bech32FromHex :: Lib -> String -> Effect (Maybe Bech32)
bech32FromHex lib str = case HexString.hex str <#> HexString.decode of
  Just bytes -> bech32FromBytes lib bytes
  Nothing -> pure Nothing

foreign import isValidBech32Impl :: EffectFn2 Lib String Boolean

isValidBech32 :: Lib -> String -> Effect Boolean
isValidBech32 lib str = runEffectFn2 isValidBech32Impl lib str

