module Contrib.Data.Foldable where

import Prelude

import Data.Foldable (class Foldable, foldMap)

foldMapFlipped :: forall a b t. Foldable t => Monoid b => t a -> (a -> b) -> b
foldMapFlipped = flip foldMap

