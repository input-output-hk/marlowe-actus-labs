module Contrib.Data.Map where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Profunctor.Strong ((&&&))

fromFoldableBy :: forall f k v. Functor f => Foldable f => Ord k => (v -> k) -> f v -> Map k v
fromFoldableBy f = Map.fromFoldable <<< map (f &&& identity)

additions :: forall k v. Ord k => Map k v -> Map k v -> Map k v
additions = flip Map.difference

deletions :: forall k v. Ord k => Map k v -> Map k v -> Map k v
deletions = Map.difference

updates :: forall k v. Eq v => Ord k => Map k v -> Map k v -> Map k { old :: v, new :: v }
updates prev = Map.filter (\{ old, new } -> old /= new) <<< Map.intersectionWith { old: _, new: _ } prev
