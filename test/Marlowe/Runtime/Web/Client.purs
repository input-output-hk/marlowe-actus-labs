module Test.Marlowe.Web.Client where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Debug (traceM)
import Effect.Aff (delay)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Marlowe.Runtime.Web.Client (foldMapMPages, foldMapMPages', getResource)
import Marlowe.Runtime.Web.Types (ContractsEndpoint(..), PostContractsResponse(..), ServerURL(..), Tx(..), GetContractsResponse, api)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: ServerURL -> Spec Unit
spec serverUrl@(ServerURL serverUrlStr) = do
  describe ("Testing web client against " <> serverUrlStr) do
     it "contracts" do
        traceM "TESTING CONTRACTS FETCH"
        contracts <- foldMapMPages' serverUrl api (pure <<< _.page) `catchError` \err -> do
          traceM $ "ERROR: " <> show err
          throwError err
        traceM "FETCHED?"
        case head <$> contracts of
            Right contractHeader -> do
              traceM contractHeader
              pure unit
              -- contract <- fetchContract serverUrl contractHeader.links.contract
              -- transactionHeaders <- fetchTransactionHeaders serverUrl contract.links.transactions
              -- case head transactionHeaders of
              --  Just transactionHeader -> do
              --     transaction <- fetchTransaction serverUrl transactionHeader.links.transaction
              --     let (Tx tx) = transaction.resource
              --     case tx.block of
              --            Just _ -> pure unit
              --            _ -> fail "Expected block"
              --  _ -> fail "Expected transaction"
            _ -> fail "Expected contract"
