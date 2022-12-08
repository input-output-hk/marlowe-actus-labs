module Wallet
  ( Api(..)
  , ApiVersion(..)
  , Cardano
  , Icon(..)
  , Name(..)
  , apis
  , cardano
  ) where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Either (either)
import Data.Either as Either
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception (throw)
import Foreign (Foreign, ForeignError(..))
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import Foreign.Object (Object)
import Foreign.Object as Object
import Web.HTML (Window)

newtype ApiVersion = ApiVersion String
newtype Icon = Icon String
newtype Name = Name String

data Api = Api
  { apiVersion :: ApiVersion
  , enable :: Aff Foreign
  , icon :: Icon
  , isEnabled :: Aff Boolean
  , name :: Name
  }

foreign import data Cardano :: Type

throwForeignErrors :: forall a. ExceptT (NonEmptyList ForeignError) Effect a -> Effect a
throwForeignErrors = either (throw <<< show <<< map Foreign.renderForeignError) pure <=< runExceptT

cardano :: Window -> Effect (Maybe Cardano)
cardano w = do
  let w_ = Foreign.unsafeToForeign w
  throwForeignErrors do
    void $ Foreign.unsafeReadTagged "Object" w_
    c :: Foreign <- Foreign.Index.readProp "cardano" w_
    if (Foreign.isUndefined c) then pure Nothing
    else do
      void $ Foreign.unsafeReadTagged "Object" c
      pure $ Just $ Foreign.unsafeFromForeign c

apis :: Cardano -> Effect (Array Api)
apis c = do
  o :: Object Foreign <- throwForeignErrors $ Foreign.unsafeReadTagged "Object" $ Foreign.unsafeToForeign c
  Array.mapMaybe Either.hush <$> traverse (runExceptT <<< readApi) (Object.values o)

readApi :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Api
readApi x = do
  void $ Foreign.unsafeReadTagged "Object" x
  apiVersion :: ApiVersion <- readApiVersion =<< Foreign.Index.readProp "apiVersion" x
  enable :: Aff Foreign <- readEnable =<< Foreign.Index.readProp "enable" x
  icon :: Icon <- readIcon =<< Foreign.Index.readProp "icon" x
  isEnabled :: Aff Boolean <- readIsEnabled =<< Foreign.Index.readProp "isEnabled" x
  name :: Name <- readName =<< Foreign.Index.readProp "name" x
  pure $ Api { apiVersion, enable, icon, isEnabled, name }
  where
  readApiVersion :: Foreign -> ExceptT (NonEmptyList ForeignError) m ApiVersion
  readApiVersion = map ApiVersion <<< Foreign.readString

  readIcon :: Foreign -> ExceptT (NonEmptyList ForeignError) m Icon
  readIcon = map Icon <<< Foreign.readString

  readName :: Foreign -> ExceptT (NonEmptyList ForeignError) m Name
  readName = map Name <<< Foreign.readString

  readEnable :: Foreign -> ExceptT (NonEmptyList ForeignError) m (Aff Foreign)
  readEnable x' = do
    f :: Effect (Promise Foreign) <- Foreign.unsafeReadTagged "Function" x'
    arity :: Int <- Foreign.readInt =<< Foreign.Index.readProp "length" x'
    unless (arity == 0) $ Foreign.fail $ ForeignError "Expected procedure of arity 0"
    pure $ toAffE f

  readIsEnabled :: Foreign -> ExceptT (NonEmptyList ForeignError) m (Aff Boolean)
  readIsEnabled x' = do
    f :: Effect (Promise Boolean) <- Foreign.unsafeReadTagged "Function" x'
    arity :: Int <- Foreign.readInt =<< Foreign.Index.readProp "length" x'
    unless (arity == 0) $ Foreign.fail $ ForeignError "Expected procedure of arity 0"
    pure $ toAffE f
