module Contrib.Data.String where

import Prelude

import Data.Enum (class BoundedEnum, upFrom)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.String (Pattern, stripPrefix)

tryStripPrefix :: Pattern -> String -> String
tryStripPrefix pattern str = fromMaybe str (stripPrefix pattern str)

decodeEnumWith :: forall a. Show a => BoundedEnum a => (String -> String) -> String -> Maybe a
decodeEnumWith adaptConstructorName = do
  let
    -- Let's precompute this `Map`
    values = Map.fromFoldable <<< map (adaptConstructorName <<< show &&& identity) $ (upFrom bottom :: Array a)
  flip Map.lookup values

encodeEnumWith :: forall a. Show a => (String -> String) -> a -> String
encodeEnumWith adaptConstructorName = adaptConstructorName <<< show
