module Test.Marlowe.Web.Client where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Marlowe.Runtime.Web.Client (fetchContract, fetchContractHeaders, fetchTransaction, fetchTransactionHeaders)
import Marlowe.Runtime.Web.Types (ServerURL(..), Tx(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: ServerURL -> Spec Unit
spec serverUrl@(ServerURL serverUrlStr) = do
  describe ("Testing web client against " <> serverUrlStr) do
     it "contracts" do
        contracts <- fetchContractHeaders serverUrl
        case head contracts of
            Just contractHeader -> do
              contract <- fetchContract serverUrl contractHeader.links.contract
              transactionHeaders <- fetchTransactionHeaders serverUrl contract.links.transactions
              case head transactionHeaders of
               Just transactionHeader -> do
                  transaction <- fetchTransaction serverUrl transactionHeader.links.transaction
                  let (Tx tx) = transaction.resource
                  case tx.block of
                         Just _ -> pure unit
                         _ -> fail "Expected block"
               _ -> fail "Expected transaction"
            _ -> fail "Expected contract"
