module Wallet.Internal where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, ForeignError)
import Foreign as Foreign
import Foreign.Index as Foreign.Index
import Foreign.Object (Object)

readCardano :: forall m. Monad m => Foreign -> ExceptT (NonEmptyList ForeignError) m (Maybe (Object Foreign))
readCardano w = do
  c :: Foreign <- Foreign.Index.readProp "cardano" w
  if (Foreign.isUndefined c) then pure Nothing
  else do
    void $ Foreign.unsafeReadTagged "Object" c
    pure $ Just $ Foreign.unsafeFromForeign c
