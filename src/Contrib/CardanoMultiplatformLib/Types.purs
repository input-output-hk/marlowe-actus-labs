module CardanoMultiplatformLib.Types
  ( JsonString
  , jsonStringToString
  , jsonStringFromString
  , unsafeJsonString
  ) where

import Prelude

import Data.Argonaut (parseJson, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe)

-- We load the lib dynamically (nodejs vs browser version) so we represent
-- the lib module by opaque value.
-- FIXME: in `js-object` we should introduce the concept of `TsClass` so we
-- can actually handle these scenarios generically.
-- foreign import data Lib :: Type

newtype JsonString = JsonString String

unsafeJsonString :: String -> JsonString
unsafeJsonString = JsonString

jsonStringFromString :: String -> Maybe JsonString
jsonStringFromString = map (JsonString <<< stringify) <<< hush <<< parseJson

jsonStringToString :: JsonString -> String
jsonStringToString (JsonString s) = s
