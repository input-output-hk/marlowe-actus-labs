module Marlowe.Runtime.Web where

import Prelude

import Contrib.Data.Argonaut (JsonParser, JsonParserResult, decodeFromString)
import Contrib.Data.Argonaut.Generic.Record (decodeNewtypedRecord)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Object

newtype TxId = TxId String

derive instance Generic TxId _
derive instance Newtype TxId _
derive instance Eq TxId
derive instance Ord TxId
instance DecodeJson TxId where
  decodeJson = genericDecodeJson

newtype TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: Int
  }

derive instance Generic TxOutRef _
derive instance Eq TxOutRef
derive instance Newtype TxOutRef _
instance DecodeJson TxOutRef where
  decodeJson = decodeFromString $ String.split (String.Pattern "#") >>> case _ of
    [ txId, txIxStr ] -> do
      txIx <- Int.fromString txIxStr
      pure $ TxOutRef { txId: TxId txId, txIx }
    _ -> Nothing

txOutRefFromString :: String -> Maybe TxOutRef
txOutRefFromString = String.split (String.Pattern "#") >>> case _ of
  [ txId, txIxStr ] -> do
    txIx <- Int.fromString txIxStr
    pure $ TxOutRef { txId: TxId txId, txIx }
  _ -> Nothing

txOutRefToString :: TxOutRef -> String
txOutRefToString (TxOutRef { txId: TxId txId, txIx }) = txId <> "#" <> show txIx

newtype PolicyId = PolicyId String

derive instance Generic PolicyId _
derive instance Newtype PolicyId _
derive instance Eq PolicyId
derive instance Ord PolicyId
instance DecodeJson PolicyId where
  decodeJson = map PolicyId <$> decodeJson

data MarloweVersion = V1

derive instance Generic MarloweVersion _
derive instance Eq MarloweVersion
derive instance Ord MarloweVersion
instance DecodeJson MarloweVersion where
  decodeJson = decodeFromString case _ of
    "v1" -> Just V1
    _ -> Nothing

data TxStatus
  = Unsigned
  | Submitted
  | Confirmed

derive instance Eq TxStatus
derive instance Ord TxStatus

--  deriving (Show, Eq, Ord)

instance DecodeJson TxStatus where
  decodeJson = decodeFromString case _ of
    "unsigned" -> Just Unsigned
    "submitted" -> Just Submitted
    "confirmed" -> Just Confirmed
    _ -> Nothing

newtype BlockHeader = BlockHeader
  { slotNo :: Int
  , blockNo :: Int
  , blockHeaderHash :: String
  }

derive instance Generic BlockHeader _
derive instance Newtype BlockHeader _
derive instance Eq BlockHeader
derive instance Ord BlockHeader
instance DecodeJson BlockHeader where
  decodeJson json = BlockHeader <$> decodeJson json

-- FIXME: We want to make it more concrete soon ;-)
type Metadata = Map Int (Object Json)

newtype ContractHeader = ContractHeader
  { contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , metadata :: Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }

derive instance Generic ContractHeader _
derive instance Newtype ContractHeader _
derive instance Eq ContractHeader

instance DecodeJson ContractHeader where
  decodeJson = do
    let
      decodeMetadata :: JsonParser Metadata
      decodeMetadata json = do
        (obj :: Object (Object Json)) <- decodeJson json

        (arr :: Array (Int /\ Object Json)) <- for (Object.toUnfoldable obj) \(idx /\ value) -> do
          idx' <- do
            let
              err = TypeMismatch $ "Expecting an integer metadata label but got: " <> show idx
            note err $ Int.fromString idx
          pure (idx' /\ value)

        pure $ Map.fromFoldable arr
    decodeNewtypedRecord { metadata: map decodeMetadata :: Maybe Json -> Maybe (JsonParserResult Metadata) }

