module Marlowe.Runtime.Web.Client where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Argonaut (Json, JsonDecodeError, decodeJson, fromObject, fromString, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Array (concat, singleton, (:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Traversable (for)
import Debug (traceM)
import Effect.Aff (Aff, Error, error)
import Fetch (fetch)
import Fetch.Core.Headers (Headers, toArray)
import Foreign (Foreign)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink(..), ResourceWithLinks, ServerURL(..), Tx, TxHeader, decodeResourceWithLink)
import Unsafe.Coerce (unsafeCoerce)

-- fetchConractHeaders
fetchContractHeaders :: ServerURL -> Aff (Array (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState)))
fetchContractHeaders serverUrl = do
  let res = ResourceLink "contracts"
  jsons <- fetchResources serverUrl res
  (contractsWithLinksJson :: Array (Array Json)) <- either (throwError <<< error <<< show) pure $
    for jsons \json -> do
      obj <- decodeJson json
      obj .: "results"
  for (concat contractsWithLinksJson) \contractWithLinksJson -> do
      let contract :: Either JsonDecodeError (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState))
          contract = decodeResourceWithLink (map decodeJson) contractWithLinksJson
      handleError contractWithLinksJson contract

-- fetchContract
fetchContract :: ServerURL -> ResourceLink ContractState -> Aff (ResourceWithLinks ContractState (transactions :: ResourceLink (Array TxHeader)))
fetchContract serverUrl res = do
  json <- fetchResource serverUrl res
  let
    contractState :: Either JsonDecodeError (ResourceWithLinks ContractState (transactions :: ResourceLink (Array TxHeader)))
    contractState = decodeResourceWithLink (map decodeJson) json
  handleError json contractState

-- fetchTransactionHeaders
fetchTransactionHeaders :: ServerURL -> ResourceLink (Array TxHeader) -> Aff (Array (ResourceWithLinks TxHeader (transaction :: ResourceLink Tx)))
fetchTransactionHeaders serverUrl res = do
  jsons <- fetchResources serverUrl res
  (txHeadersJsonArr :: Array (Array Json)) <- either (throwError <<< error <<< show) pure $
    for jsons \json -> do
      obj <- decodeJson json
      obj .: "results"

  for (concat txHeadersJsonArr) \txHeaderJson -> do
    let txHeader :: Either JsonDecodeError (ResourceWithLinks TxHeader (transaction :: ResourceLink Tx))
        txHeader = decodeResourceWithLink (map decodeJson) txHeaderJson
    handleError txHeaderJson txHeader

-- fetchTransaction
fetchTransaction :: ServerURL -> ResourceLink Tx -> Aff (ResourceWithLinks Tx (previous :: ResourceLink Tx))
fetchTransaction serverUrl res = do
  json <- fetchResource serverUrl res
  let
    tx :: Either JsonDecodeError (ResourceWithLinks Tx (previous :: ResourceLink Tx))
    tx = decodeResourceWithLink (map decodeJson) json
  handleError json tx

-- fetchResource
fetchResource :: forall a. ServerURL -> ResourceLink a -> Aff Json
fetchResource (ServerURL serverUrl) (ResourceLink path) = do
  let
    url = serverUrl <> path

    bringBackJson :: Foreign -> Json
    bringBackJson = unsafeCoerce

  res <- fetch url { headers: { "Accept": "application/json" } }
  bringBackJson <$> res.json

-- fetchResources
fetchResources :: forall a. ServerURL -> ResourceLink a -> Aff (Array Json)
fetchResources (ServerURL serverUrl) (ResourceLink path) = do
  let
    url = serverUrl <> path

    bringBackJson :: Foreign -> Json
    bringBackJson = unsafeCoerce

    fetchAll :: _ -> Aff (Array Json)
    fetchAll header = do
       { json, status, headers } <- fetch url header
       if status == 206
         then
            case lookup (CaseInsensitiveString "Next-Range") (toHeaders headers) of
               Just nextRange ->
                  lift2 (:)
                    (bringBackJson <$> json)
                    (fetchAll { headers: { "Accept": "application/json" , "Range": nextRange } })
               _ -> do
                 traceM json
                 throwError $ error "HTTP 206 but Next-Range header missing"
         else
            singleton <<< bringBackJson <$> json

  fetchAll { headers: { "Accept": "application/json" , "Range": "contractId" } }

-- from: Fetch.Internal.Headers
toHeaders :: Headers -> Map CaseInsensitiveString String
toHeaders = toArray >>> map (lmap CaseInsensitiveString) >>> fromFoldable

-- handleError
handleError :: forall a b c. Show b => MonadThrow Error a => Json -> Either b c -> a c
handleError json (Left err) =
  let errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
   in throwError $ error $ stringify errJson
handleError _ (Right c) = pure c
