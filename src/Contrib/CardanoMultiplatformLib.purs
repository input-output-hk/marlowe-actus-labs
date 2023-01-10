module CardanoMultiplatformLib
  ( importLib
  , module Exports
  , GarbageCollector
  , allocate
  , runGarbageCollector
  ) where

import Prelude

import CardanoMultiplatformLib.Lib (Lib)
import CardanoMultiplatformLib.Lib (Lib) as Exports
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (catchError)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask)
import Data.Foldable (sequence_)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
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

-- | StateT is not sufficient because it is not exception
-- | safe. We need to use `Ref` to store the release actions.
newtype GarbageCollector a = GarbageCollector
  (ReaderT (Ref (List (Effect Unit))) Effect a)

runGarbageCollector :: forall a. GarbageCollector a -> Effect a
runGarbageCollector (GarbageCollector action) = do
  freesRef <- Ref.new List.Nil
  let
    release = do
      frees <- Ref.read freesRef
      sequence_ frees
    run = do
      a <- runReaderT action freesRef
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
  freesRef <- ask
  obj <- liftEffect alloc
  let
    jsobj = unwrap obj
    _free = Proxy :: Proxy "free"
  liftEffect $ Ref.modify_ (List.Cons (runEffectMth0 _free jsobj)) freesRef
  pure obj
