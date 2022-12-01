module Marlowe.Runtime.Web.Client where

import Prelude

import Control.Monad.Except (throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson, printJsonDecodeError)
import Data.Either (Either, either)
import Effect.Aff (Aff, error)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Foreign (Foreign)
import Marlowe.Runtime.Web.Types (ResourceLink(..), ServerURL(..))
import Unsafe.Coerce (unsafeCoerce)

-- fetchConractHeaders ..
-- 
-- fetchContract
-- 
-- fetchTransactionHeaders
-- 
-- fetchTransaction

fetchResource :: forall a. DecodeJson a => ServerURL -> ResourceLink a -> Aff a
fetchResource (ServerURL serverUrl) (ResourceLink path) = do
  let
    url = serverUrl <> path

    bringBackJson :: Foreign -> Json
    bringBackJson = unsafeCoerce

  res <- fetch url { headers: { "Accept": "application/json" } }
  json <- bringBackJson <$> res.json

  either
    (throwError <<< error <<< printJsonDecodeError)
    pure
    (decodeJson json)

