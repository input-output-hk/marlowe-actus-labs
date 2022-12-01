module Marlowe.Runtime.Web.Types where

import Prelude

import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn, decodeRecord)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype ResourceLink :: Type -> Type
newtype ResourceLink resource = ResourceLink String

derive instance Generic (ResourceLink resource) _
derive instance Newtype (ResourceLink resource) _
derive instance Eq (ResourceLink resource)
derive instance Ord (ResourceLink resource)
instance DecodeJson (ResourceLink resource) where
  decodeJson json = ResourceLink <$> decodeJson json

type ResourceWithLinksRow resource linksRow =
  ( links :: { | linksRow }
  , resource :: resource
  )

type ResourceWithLinks :: Type -> Row Type -> Type
type ResourceWithLinks resource linksRow = { | ResourceWithLinksRow resource linksRow }

decodeResourceWithLink
  :: forall a linksRow
   . DecodeRecord (resource :: DecodeJsonFieldFn a) (ResourceWithLinksRow a linksRow)
  => DecodeJsonFieldFn a
  -> Json
  -> Either JsonDecodeError (ResourceWithLinks a linksRow)
decodeResourceWithLink decodeResource = decodeRecord { resource: decodeResource }
