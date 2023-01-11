module CardanoMultiplatformLib
  ( importLib
  , module Exports
  , GarbageCollector
  , allocate
  , runGarbageCollector
  , transactionWitnessSetFromBytes
  ) where

import Prelude

import CardanoMultiplatformLib.Lib (Lib)
import CardanoMultiplatformLib.Lib (Lib) as Exports
import CardanoMultiplatformLib.Lib as Lib
import CardanoMultiplatformLib.Transaction (TransactionWitnessSetObject(..))
import CardanoMultiplatformLib.Transaction as Transaction
import CardanoMultiplatformLib.Types (Cbor, CborHex(..))
import CardanoMultiplatformLib.Types (CborHex(..)) as Exports
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask, asks)
import Data.Foldable (length, sequence_)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HexString as HexString
import JS.Object (EffectMth0, JSObject, runEffectMth0)
import Promise.Aff (Promise, toAff)
import Type.Prelude (Proxy(..))

-- TODO: Move to Lib module
foreign import importLibImpl :: Effect (Nullable (Promise Lib))

importLib :: Aff (Maybe Lib)
importLib = liftEffect importLibImpl >>= Nullable.toMaybe >>> case _ of
  Nothing -> pure Nothing
  Just promise ->
    (Just <$> toAff promise) `catchError` const (pure Nothing)

type Ctx = { lib :: Lib, frees :: Ref (List (Effect Unit)) }

-- | StateT is not sufficient because it is not exception
-- | safe. We need to use `Ref` to store the release actions.
newtype GarbageCollector a = GarbageCollector
  (ReaderT Ctx Effect a)
derive newtype instance Functor GarbageCollector
derive newtype instance Apply GarbageCollector
derive newtype instance Applicative GarbageCollector
derive newtype instance Bind GarbageCollector
derive newtype instance Monad GarbageCollector
derive newtype instance MonadEffect GarbageCollector

runGarbageCollector :: forall a. Lib -> GarbageCollector a -> Effect a
runGarbageCollector lib (GarbageCollector action) = do
  freesRef <- Ref.new List.Nil
  let
    release = do
      frees <- Ref.read freesRef
      traceM $ "Releasing " <> show (length frees :: Int) <> " resources"
      sequence_ frees
    run = do
      a <- runReaderT action { frees: freesRef, lib }
      release
      pure a
  run `catchError` \err -> do
    release
    throwError err

-- | The API allocates objects which provide `free` method.
-- | We use it to release the resources.
type UnmanagedObject r = JSObject (free :: EffectMth0 Unit | r)

allocate :: forall r t. Newtype t (UnmanagedObject r) => Effect t -> GarbageCollector t
allocate alloc = GarbageCollector do
  freesRef <- asks _.frees
  obj <- liftEffect alloc
  let
    jsobj = unwrap obj
    _free = Proxy :: Proxy "free"
  liftEffect $ Ref.modify_ (List.Cons (runEffectMth0 _free jsobj)) freesRef
  pure obj

transactionWitnessSetFromBytes :: Cbor TransactionWitnessSetObject -> GarbageCollector TransactionWitnessSetObject
transactionWitnessSetFromBytes twCbor = do
  lib <- GarbageCollector $ asks _.lib
  let
    { "TransactionWitnessSet": tws } = Lib.props lib
    { "from_bytes": fromBytes } = Transaction.transactionWitnessSet
  allocate $ fromBytes tws twCbor


