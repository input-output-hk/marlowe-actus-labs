module Marlowe.Runtime.Web.Types where

import Prelude

import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn, decodeRecord)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Type.Row.Homogeneous (class Homogeneous)

newtype ResourceLink = ResourceLink String
derive instance Generic ResourceLink _
derive instance Newtype ResourceLink _
derive instance Eq ResourceLink
derive instance Ord ResourceLink
instance DecodeJson ResourceLink where
  decodeJson json= ResourceLink <$> decodeJson json

type ResourceWithLinksRow resource linksRow =
  ( links :: { | linksRow }
  , resource :: resource
  )

type ResourceWithLinks resource linksRow = { | ResourceWithLinksRow resource linksRow }

decodeResourceWithLink
  :: forall a linksRow
   . Homogeneous linksRow ResourceLink
  => DecodeRecord (resource :: DecodeJsonFieldFn a) (ResourceWithLinksRow a linksRow)
  => DecodeJsonFieldFn a
  -> Json
  -> Either JsonDecodeError (ResourceWithLinks a linksRow)
decodeResourceWithLink decodeResource = decodeRecord { resource: decodeResource }
