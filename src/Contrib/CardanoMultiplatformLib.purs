module CardanoMultiplatformLib
  ( importLib
  , module Exports
  ) where

import Prelude

import CardanoMultiplatformLib.Lib (Lib)
import CardanoMultiplatformLib.Lib (Lib) as Exports
import Control.Monad.Except (catchError)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Promise.Aff (Promise, toAff)

-- TODO: Move to Lib module
foreign import importLibImpl :: Effect (Nullable (Promise Lib))

importLib :: Aff (Maybe Lib)
importLib = liftEffect importLibImpl >>= Nullable.toMaybe >>> case _ of
  Nothing -> pure Nothing
  Just promise ->
    (Just <$> toAff promise) `catchError` const (pure Nothing)

