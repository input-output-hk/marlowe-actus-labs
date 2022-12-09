module Wallet where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (either)
import Data.List.Types (NonEmptyList)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (ForeignError)
import Foreign as Foreign

throwForeignErrors :: forall a. ExceptT (NonEmptyList ForeignError) Effect a -> Effect a
throwForeignErrors = either (throw <<< show <<< map Foreign.renderForeignError) pure <=< runExceptT

-- apis :: Cardano -> Effect (Array Wallet)
-- apis c = do
--   o :: Object Foreign <- throwForeignErrors $ Foreign.unsafeReadTagged "Object" $ Foreign.unsafeToForeign c
--   Array.mapMaybe Either.hush <$> traverse (runExceptT <<< readApi) (Object.values o)

-- readApi :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m Wallet
-- readApi x = do
--   void $ Foreign.unsafeReadTagged "Object" x
--   apiVersion :: ApiVersion <- readApiVersion =<< Foreign.Index.readProp "apiVersion" x
--   enable :: Aff Api <- readEnable =<< Foreign.Index.readProp "enable" x
--   icon :: Icon <- readIcon =<< Foreign.Index.readProp "icon" x
--   isEnabled :: Aff Boolean <- readIsEnabled =<< Foreign.Index.readProp "isEnabled" x
--   name :: Name <- readName =<< Foreign.Index.readProp "name" x
--   pure $ Wallet { apiVersion, enable, icon, isEnabled, name }
--   where
--   readApiVersion :: Foreign -> ExceptT (NonEmptyList ForeignError) m ApiVersion
--   readApiVersion = map ApiVersion <<< Foreign.readString

--   readIcon :: Foreign -> ExceptT (NonEmptyList ForeignError) m Icon
--   readIcon = map Icon <<< Foreign.readString

--   readName :: Foreign -> ExceptT (NonEmptyList ForeignError) m Name
--   readName = map Name <<< Foreign.readString

--   readEnable :: Foreign -> ExceptT (NonEmptyList ForeignError) m (Aff Api)
--   readEnable x' = do
--     f :: Effect (Promise Api) <- Foreign.unsafeReadTagged "Function" x'
--     arity :: Int <- Foreign.readInt =<< Foreign.Index.readProp "length" x'
--     unless (arity == 0) $ Foreign.fail $ ForeignError "Expected procedure of arity 0"
--     pure $ toAffE f

--   readIsEnabled :: Foreign -> ExceptT (NonEmptyList ForeignError) m (Aff Boolean)
--   readIsEnabled x' = do
--     f :: Effect (Promise Boolean) <- Foreign.unsafeReadTagged "Function" x'
--     arity :: Int <- Foreign.readInt =<< Foreign.Index.readProp "length" x'
--     unless (arity == 0) $ Foreign.fail $ ForeignError "Expected procedure of arity 0"
--     pure $ toAffE f
