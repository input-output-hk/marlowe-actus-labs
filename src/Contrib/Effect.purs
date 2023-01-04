module Contrib.Effect where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (throwError)
import Effect (Effect)

bracket :: forall a b. Effect a -> (a -> Effect Unit) -> (a -> Effect b) -> Effect b
bracket acquire release action = do
  resource <- acquire
  b <- action resource `catchError` \error -> do
    void $ release resource
    throwError error
  release resource
  pure b
