module Marlowe.Runtime.Web where

import Prelude

import Contrib.Data.Argonaut (JsonParser, JsonParserResult, decodeFromString)
import Contrib.Data.Argonaut.Generic.Record (DecodeJsonFieldFn, decodeNewtypedRecord)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Decode.Decoders (decodeMaybe)
import Data.DateTime (DateTime)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Record as Record

newtype TxId = TxId String

derive instance Generic TxId _
derive instance Newtype TxId _
derive instance Eq TxId
derive instance Ord TxId
instance DecodeJson TxId where
  decodeJson = map TxId <$> decodeJson

newtype TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: Int
  }

derive instance Generic TxOutRef _
derive instance Eq TxOutRef
derive instance Ord TxOutRef
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

type ContractHeaderRowBase r =
  ( contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , metadata :: Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  | r
  )

newtype ContractHeader = ContractHeader { | ContractHeaderRowBase () }

derive instance Generic ContractHeader _
derive instance Newtype ContractHeader _
derive instance Eq ContractHeader

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

metadataFieldDecoder :: { metadata :: DecodeJsonFieldFn Metadata }
metadataFieldDecoder = { metadata: map decodeMetadata :: Maybe Json -> Maybe (JsonParserResult Metadata) }

instance DecodeJson ContractHeader where
  decodeJson = decodeNewtypedRecord metadataFieldDecoder

-- FIXME: The belowe two types are just stubs for now - we can probably use them using serialization lib bindings or transaction-lib:
-- * https://github.com/Emurgo/cardano-serialization-lib
-- * https://github.com/Plutonomicon/cardano-transaction-lib
newtype TextEnvelope (a :: Type) = TextEnvelope String

derive instance Generic (TextEnvelope a) _
derive instance Newtype (TextEnvelope a) _
derive instance Eq (TextEnvelope a)

-- We don't loook under the hood so it is a bit "unsafe"
unsafeDecodeTextEnvelope :: forall a. JsonParser (TextEnvelope a)
unsafeDecodeTextEnvelope = map TextEnvelope <$> decodeJson

decodeTxBodyTextEnvelope :: JsonParser (TextEnvelope TxBody)
decodeTxBodyTextEnvelope = unsafeDecodeTextEnvelope

newtype TxBody = TxBody String

derive instance Generic TxBody _
derive instance Newtype TxBody _
derive instance Eq TxBody

type ContractStateRow = ContractHeaderRowBase
  ( initialContract :: V1.Contract
  , currentContract :: Maybe V1.Contract
  , state :: Maybe V1.State
  , utxo :: Maybe TxOutRef
  , txBody :: Maybe (TextEnvelope TxBody)
  )

newtype ContractState = ContractState { | ContractStateRow }

derive instance Generic ContractState _
derive instance Newtype ContractState _
derive instance Eq ContractState

instance DecodeJson ContractState where
  decodeJson = decodeNewtypedRecord decoders
    where
    decoders =
      metadataFieldDecoder
        `Record.merge`
          { txBody: map (decodeMaybe decodeTxBodyTextEnvelope) :: Maybe _ -> Maybe _ }

type TxRowBase r =
  ( contractId :: TxOutRef
  , transactionId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  | r
  )

type TxHeaderRow = TxRowBase (utxo :: Maybe TxOutRef)
newtype TxHeader = TxHeader { | TxHeaderRow }

derive instance Generic TxHeader _
derive instance Newtype TxHeader _
derive instance Eq TxHeader
derive instance Ord TxHeader
instance DecodeJson TxHeader where
  decodeJson = map TxHeader <$> decodeJson

decodeUTCDateTime :: Json -> Either JsonDecodeError DateTime
decodeUTCDateTime json = do
  str <- decodeJson json
  -- We handle only a string with "Z" suffix
  -- at the end ("Z(ero)" time shift)
  note (UnexpectedValue json) $ do
    -- This is `hasSuffix` check equivalent.
    _ <- String.stripSuffix (String.Pattern "Z") str
    let
      jsDate = unsafePerformEffect $ JSDate.parse $ str
    JSDate.toDateTime jsDate

type TxRow = TxRowBase
  ( inputUtxo :: TxOutRef
  , inputContract :: V1.Contract
  , inputState :: V1.State
  , inputs :: Array V1.Input
  , outputUtxo :: Maybe TxOutRef
  , outputContract :: Maybe V1.Contract
  , outputState :: Maybe V1.State
  , consumingTx :: Maybe TxId
  , invalidBefore :: DateTime
  , invalidHereafter :: DateTime
  , txBody :: Maybe (TextEnvelope TxBody)
  )

newtype Tx = Tx { | TxRow }

derive instance Generic Tx _
derive instance Newtype Tx _
derive instance Eq Tx
instance DecodeJson Tx where
  decodeJson = do
    decodeNewtypedRecord
      { invalidBefore: map decodeUTCDateTime :: Maybe _ -> Maybe _
      , invalidHereafter: map decodeUTCDateTime :: Maybe _ -> Maybe _
      , txBody: map (decodeMaybe decodeTxBodyTextEnvelope) :: Maybe _ -> Maybe _
      }
