module Marlowe.Runtime.Web.Client where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (throwError)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, fromObject, fromString, printJsonDecodeError, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..), either)
import Data.Traversable (for)
import Effect.Aff (Aff, Error, error)
import Fetch (fetch)
import Fetch as RequestMode
import Foreign (Foreign)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (ContractHeader, ContractState, ResourceLink(..), ResourceWithLinks, ServerURL(..), Tx, TxHeader, decodeResourceWithLink)
import Unsafe.Coerce (unsafeCoerce)

-- fetchConractHeaders
fetchContractHeaders :: ServerURL -> String -> Aff (Array (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState)))
fetchContractHeaders serverUrl location = do
  let
    res = ResourceLink location
  json <- fetchResource serverUrl res
  (contractsWithLinksJson :: Array Json) <- either (throwError <<< error <<< show) pure do
    obj <- decodeJson json
    obj .: "results"

  for contractsWithLinksJson \contractWithLinksJson -> do
      let contract :: Either JsonDecodeError (ResourceWithLinks ContractHeader (contract :: ResourceLink ContractState))
          contract = decodeResourceWithLink (map decodeJson) contractWithLinksJson
      handleError contractWithLinksJson contract

-- fetchContract
fetchContract :: ServerURL -> String -> Aff (ResourceWithLinks ContractState (transactions :: ResourceLink (Array TxHeader)))
fetchContract serverUrl location = do
  let
    res = ResourceLink location
  json <- fetchResource serverUrl res

  let
    contractState :: Either JsonDecodeError (ResourceWithLinks ContractState (transactions :: ResourceLink (Array TxHeader)))
    contractState = decodeResourceWithLink (map decodeJson) json
  handleError json contractState

-- fetchTransactionHeaders
fetchTransactionHeaders :: ServerURL -> String -> Aff (Array (ResourceWithLinks TxHeader (transaction :: ResourceLink Tx)))
fetchTransactionHeaders serverUrl location = do
  let
    res = ResourceLink location
  json <- fetchResource serverUrl res
  (txHeadersJsonArr :: Array Json) <- either (throwError <<< error <<< show) pure do
      obj <- decodeJson json
      obj .: "results"

  for txHeadersJsonArr \txHeaderJson -> do
    let txHeader :: Either JsonDecodeError (ResourceWithLinks TxHeader (transaction :: ResourceLink Tx))
        txHeader = decodeResourceWithLink (map decodeJson) txHeaderJson
    handleError txHeaderJson txHeader

-- fetchTransaction
fetchTransaction :: ServerURL -> String -> Aff (ResourceWithLinks Tx (previous :: ResourceLink Tx))
fetchTransaction serverUrl location = do
  let
    res = ResourceLink location
  json <- fetchResource serverUrl res

  let
    tx :: Either JsonDecodeError (ResourceWithLinks Tx (previous :: ResourceLink Tx))
    tx = decodeResourceWithLink (map decodeJson) json
  handleError json tx

-- fetchResource
fetchResource :: forall a. DecodeJson a => ServerURL -> ResourceLink a -> Aff a
fetchResource (ServerURL serverUrl) (ResourceLink path) = do
  let
    url = serverUrl <> path

    bringBackJson :: Foreign -> Json
    bringBackJson = unsafeCoerce

  res <- fetch url { headers: { "Accept": "application/json" } , mode: RequestMode.NoCors }
  json <- bringBackJson <$> res.json

  either
    (throwError <<< error <<< printJsonDecodeError)
    pure
    (decodeJson json)

-- handleError
handleError :: forall a b c. Show b => MonadThrow Error a => Json -> Either b c -> a c
handleError json (Left err) =
  let errJson = fromObject $ Object.fromHomogeneous { json, err: fromString $ show err }
   in throwError $ error $ stringify errJson
handleError _ (Right c) = pure c
