module Marlowe.Runtime.Web.Client where

import Prelude

import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn)
import Contrib.Fetch (FetchError, fetchEither, jsonBody)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Array (fold)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch (RequestMode(..))
import Fetch.Core.Headers (Headers, toArray)
import Foreign.Object (Object)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (class EncodeHeaders, class EncodeJsonBody, class ToResourceLink, IndexEndpoint(..), ResourceEndpoint(..), ResourceLink(..), ResourceWithLinks, ServerURL(..), ResourceWithLinksRow, decodeResourceWithLink, encodeHeaders, encodeJsonBody, toResourceLink)
import Prim.Row (class Lacks) as Row
import Record as R
import Type.Prelude (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row

data ClientError
  = FetchError FetchError
  | ResponseDecodingError JsonDecodeError

derive instance Generic ClientError _

instance Show ClientError where
  show = genericShow

type GetResourceResponse res = Either ClientError res

allowedStatusCodes :: Array Int
allowedStatusCodes = [ 200, 201, 206 ]

decodeResponse :: forall a. DecodeJson a => JsonParser a
decodeResponse json = do
  obj <- decodeJson json
  res <- obj .: "results"
  decodeJson res

newtype Range = Range String

getResource
  :: forall a extraHeaders
   . DecodeJson a
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Accept" :: String, "Access-Control-Request-Headers" :: String | extraHeaders) String
  => ServerURL
  -> ResourceLink a
  -> { | extraHeaders }
  -> Aff (GetResourceResponse { headers :: Headers, payload :: a, status :: Int })
getResource (ServerURL serverUrl) (ResourceLink path) extraHeaders = do
  let
    url = serverUrl <> "/" <> path

    reqHeaders =
      R.insert (Proxy :: Proxy "Access-Control-Request-Headers") "Range, Accept"
        $ R.insert (Proxy :: Proxy "Accept") "application/json"
        $ extraHeaders

  runExceptT do
    res@{ status, headers: resHeaders } <- ExceptT $ fetchEither url { headers: reqHeaders, mode: Cors } allowedStatusCodes FetchError
    lift (jsonBody res) >>= decodeResponse >>> case _ of
      Left err -> throwError (ResponseDecodingError err)
      Right payload -> pure { payload, headers: resHeaders, status }

getPage
  :: forall a
   . DecodeJson a
  => ServerURL
  -> ResourceLink a
  -> Maybe Range
  -> Aff (GetResourceResponse ({ page :: a, nextRange :: Maybe Range }))
getPage serverUrl path possibleRange = runExceptT do
  { headers, payload, status } <- ExceptT case possibleRange of
    Nothing ->
      getResource serverUrl path { "Range": "contractId" }
    Just (Range range) -> do
      res <- getResource serverUrl path { "Range": range }
      pure res
  let
    toHeaders :: Headers -> Map CaseInsensitiveString String
    toHeaders = toArray >>> map (lmap CaseInsensitiveString) >>> fromFoldable

    nextRange = case status, lookup (CaseInsensitiveString "Next-Range") (toHeaders headers) of
      206, Just nr -> Just (Range nr)
      _, _ -> Nothing
  pure { page: payload, nextRange }

data FoldPageStep = FetchPage (Maybe Range) | StopFetching

foldMapMPages
  :: forall a b m
   . DecodeJson a
  => MonadAff m
  => Monoid b
  => ServerURL
  -> ResourceLink a
  -> ({ page :: a, currRange :: Maybe Range } -> m b)
  -> m (GetResourceResponse b)
foldMapMPages serverUrl path f = do
  let
    seed = FetchPage Nothing
  bs <- runExceptT $ flip unfoldrM seed case _ of
    StopFetching -> pure Nothing
    FetchPage currRange -> do
      { page, nextRange } <- ExceptT $ liftAff $ getPage serverUrl path currRange
      b <- lift $ f { page, currRange }
      case nextRange of
        Just _ -> pure $ Just (b /\ FetchPage nextRange)
        Nothing -> pure $ Just (b /\ StopFetching)
  pure (fold <$> bs)

getPages
  :: forall a m
   . DecodeJson a
  => MonadAff m
  => ServerURL
  -> ResourceLink a
  -> m
       ( GetResourceResponse
           ( List
               { page :: a
               , currRange :: Maybe Range
               }
           )
       )
getPages serverUrl path = foldMapMPages serverUrl path (pure <<< List.singleton)

getResource'
  :: forall a extraHeaders endpoint
   . DecodeJson a
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Accept" :: String, "Access-Control-Request-Headers" :: String | extraHeaders) String
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> Record extraHeaders
  -> Aff (GetResourceResponse { headers :: Headers, payload :: a, status :: Int })
getResource' serverUrl path = getResource serverUrl (toResourceLink path)

getPage'
  :: forall a endpoint
   . DecodeJson a
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> Maybe Range
  -> Aff (GetResourceResponse ({ page :: a, nextRange :: Maybe Range }))
getPage' serverUrl path = getPage serverUrl (toResourceLink path)

foldMapMPages'
  :: forall a b m t
   . DecodeJson a
  => MonadAff m
  => Monoid b
  => ToResourceLink t a
  => ServerURL
  -> t
  -> ( { currRange :: Maybe Range
       , page :: a
       }
       -> m b
     )
  -> m (Either ClientError b)
foldMapMPages' serverUrl path = foldMapMPages serverUrl (toResourceLink path)

post
  :: forall links postRequest postResponse getResponse extraHeaders
   . DecodeJson postResponse
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse links)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Accept" :: String, "Content-Type" :: String | extraHeaders) String
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> IndexEndpoint postRequest postResponse getResponse links
  -> postRequest
  -> Aff (GetResourceResponse (ResourceWithLinks postResponse links))
post (ServerURL serverUrl) (IndexEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: { "Accept" :: String, "Content-Type" :: String | extraHeaders }
    headers =
      R.insert (Proxy :: Proxy "Accept") "application/json"
        $ R.insert (Proxy :: Proxy "Content-Type") "application/json"
        $ (encodeHeaders req :: { | extraHeaders })

  traceM "Request body:"
  traceM body

  traceM "Request headers:"
  traceM headers

  response <- ExceptT $ fetchEither url { method: POST, body, headers } allowedStatusCodes FetchError
  (lift (jsonBody response)) >>= decodeResourceWithLink (map decodeJson :: Maybe _ -> Maybe _) >>> case _ of
    Left err -> throwError (ResponseDecodingError err)
    Right payload -> pure payload

post'
  :: forall t links postRequest postResponse getResponse extraHeaders
   . Newtype t (IndexEndpoint postRequest postResponse getResponse links)
  => DecodeJson postResponse
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse links)
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Accept" :: String, "Content-Type" :: String | extraHeaders) String
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> postRequest
  -> Aff (Either ClientError (ResourceWithLinks postResponse links))
post' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  post serverUrl endpoint' req

put
  :: forall links putRequest getResponse extraHeaders
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Accept" :: String, "Content-Type" :: String | extraHeaders) String
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> ResourceEndpoint putRequest getResponse links
  -> putRequest
  -> Aff (Either FetchError Unit)
put (ServerURL serverUrl) (ResourceEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: { "Accept" :: String, "Content-Type" :: String | extraHeaders }
    headers =
      R.insert (Proxy :: Proxy "Accept") "application/json"
        $ R.insert (Proxy :: Proxy "Content-Type") "application/json"
        $ (encodeHeaders req :: { | extraHeaders })
  void $ ExceptT $ fetchEither url { method: PUT, body, headers } allowedStatusCodes identity


put'
  :: forall links putRequest getResponse extraHeaders t
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Newtype t (ResourceEndpoint putRequest getResponse links)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Accept" :: String, "Content-Type" :: String | extraHeaders) String
  => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> putRequest
  -> Aff (Either FetchError Unit)
put' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  put serverUrl endpoint' req
