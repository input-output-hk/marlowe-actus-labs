module Actus.Utility.ANN.Annuity
  ( annuity
  ) where

import Data.List (List(..), foldl, (:))
import Prelude

-- |annuity amount function (A), as described in section 3.8 in the
-- ACTUS reference v1.1
annuity :: forall a. Semiring a => EuclideanRing a => a -> List a -> a
annuity r ti = numerator / denominator

  where
  numerator = _product $ map ((_ + one) <<< (_ * r)) ti
  denominator = _sum (map _product $ tails $ map ((_ + one) <<< (_ * r)) ti)

  -- note that _product [] == 1
  _product = foldl (*) one
  _sum = foldl (+) zero

  tails Nil = Nil
  tails as'@(Cons _ as) = as' : tails as
