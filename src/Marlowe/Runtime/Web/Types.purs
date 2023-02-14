module Marlowe.Runtime.Web.Types where

import Prelude

import CardanoMultiplatformLib (Bech32, CborHex, bech32ToString)
import CardanoMultiplatformLib.Transaction (TransactionObject, TransactionWitnessSetObject)
import CardanoMultiplatformLib.Types (unsafeBech32)
import Contrib.Data.Argonaut (JsonParser, JsonParserResult, decodeFromString)
import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn, decodeRecord, decodeNewtypedRecord)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Decode.Decoders (decodeMaybe)
import Data.DateTime (DateTime)
import Data.DateTime.ISO (ISO(..))
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.JSDate as JSDate
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, unwrap)
import Data.Profunctor.Strong ((***))
import Data.String as String
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Record as Record
import Type.Row (type (+))
import Type.Row.Homogeneous as Row

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

type ContractId = TxOutRef

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
instance EncodeJson MarloweVersion where
  encodeJson = encodeJson <<< case _ of
    V1 -> "v1"

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

instance Show TxStatus where
  show Unsigned = "Unsigned"
  show Submitted = "Submitted"
  show Confirmed = "Confirmed"

instance DecodeJson TxStatus where
  decodeJson = decodeFromString case _ of
    "unsigned" -> Just Unsigned
    "submitted" -> Just Submitted
    "confirmed" -> Just Confirmed
    _ -> Nothing

newtype BlockNumber = BlockNumber Int

derive instance Generic BlockNumber _
derive instance Newtype BlockNumber _
derive instance Eq BlockNumber
derive instance Ord BlockNumber
instance DecodeJson BlockNumber where
  decodeJson json = BlockNumber <$> decodeJson json

newtype SlotNumber = SlotNumber Int

derive instance Generic SlotNumber _
derive instance Newtype SlotNumber _
derive instance Eq SlotNumber
derive instance Ord SlotNumber
instance DecodeJson SlotNumber where
  decodeJson json = SlotNumber <$> decodeJson json

newtype BlockHeader = BlockHeader
  { slotNo :: SlotNumber
  , blockNo :: BlockNumber
  , blockHeaderHash :: String
  }

derive instance Generic BlockHeader _
derive instance Newtype BlockHeader _
derive instance Eq BlockHeader
derive instance Ord BlockHeader
instance DecodeJson BlockHeader where
  decodeJson json = BlockHeader <$> decodeJson json

newtype Metadata = Metadata (Map Int Json)

derive instance Generic Metadata _
derive instance Newtype Metadata _
derive instance Eq Metadata
instance Semigroup Metadata where
  append (Metadata a) (Metadata b) = Metadata (Map.union a b)

instance Monoid Metadata where
  mempty = Metadata Map.empty

instance EncodeJson Metadata where
  encodeJson = encodeJson
    <<< Object.fromFoldable
    <<< map (show *** identity)
    <<< (Map.toUnfoldable :: _ -> Array _)
    <<< un Metadata

instance DecodeJson Metadata where
  decodeJson json = do
    (obj :: Object Json) <- decodeJson json

    (arr :: Array (Int /\ Json)) <- for (Object.toUnfoldable obj) \(idx /\ value) -> do
      idx' <- do
        let
          err = TypeMismatch $ "Expecting an integer metadata label but got: " <> show idx
        note err $ Int.fromString idx
      pure (idx' /\ value)
    pure <<< Metadata <<< Map.fromFoldable $ arr

metadataFieldDecoder :: { metadata :: DecodeJsonFieldFn Metadata }
metadataFieldDecoder = { metadata: map decodeJson :: Maybe Json -> Maybe (JsonParserResult Metadata) }

type ContractHeadersRowBase r =
  ( contractId :: TxOutRef
  , roleTokenMintingPolicyId :: PolicyId
  , version :: MarloweVersion
  , metadata :: Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  | r
  )

newtype ContractHeader = ContractHeader { | ContractHeadersRowBase () }

derive instance Generic ContractHeader _
derive instance Newtype ContractHeader _
derive instance Eq ContractHeader

instance DecodeJson ContractHeader where
  decodeJson = decodeNewtypedRecord metadataFieldDecoder

newtype TextEnvelope (a :: Type) = TextEnvelope
  { type_ :: String
  , description :: String
  , cborHex :: CborHex a
  }

derive instance Generic (TextEnvelope a) _
derive instance Newtype (TextEnvelope a) _
derive instance Eq (TextEnvelope a)

instance DecodeJson (TextEnvelope a) where
  decodeJson = unsafeDecodeTextEnvelope

-- We don't loook under the hood so it is a bit "unsafe" - in a given
-- context when we know what `a` "should be" we can use this function
-- or the above instance.
unsafeDecodeTextEnvelope :: forall a. JsonParser (TextEnvelope a)
unsafeDecodeTextEnvelope json = TextEnvelope <$> do
  (obj :: Object Json) <- decodeJson json
  type_ <- obj .: "type"
  description <- obj .: "description"
  cborHex <- obj .: "cborHex"
  pure { type_, description, cborHex }

instance EncodeJson (TextEnvelope a) where
  encodeJson (TextEnvelope { type_, description, cborHex }) = encodeJson { "type": type_, description, cborHex }

decodeTransactionObjectTextEnvelope :: JsonParser (TextEnvelope TransactionObject)
decodeTransactionObjectTextEnvelope = unsafeDecodeTextEnvelope

class HasTextEnvelope :: Type -> Constraint
class HasTextEnvelope a where
  textEnvelopeType :: forall proxy. proxy a -> String

instance HasTextEnvelope TransactionWitnessSetObject where
  textEnvelopeType _ = "WitnessesBabbage"

toTextEnvelope :: forall a. HasTextEnvelope a => CborHex a -> String -> TextEnvelope a
toTextEnvelope cborHex description = TextEnvelope
  { type_: textEnvelopeType cborHex, description, cborHex }

newtype TxBody = TxBody String

derive instance Generic TxBody _
derive instance Newtype TxBody _
derive instance Eq TxBody

type ContractStateRow = ContractHeadersRowBase
  ( initialContract :: V1.Contract
  , currentContract :: Maybe V1.Contract
  , state :: Maybe V1.State
  , utxo :: Maybe TxOutRef
  , txBody :: Maybe (TextEnvelope TransactionObject)
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
          { txBody: map (decodeMaybe decodeTransactionObjectTextEnvelope) :: Maybe _ -> Maybe _ }

type TxRowBase r =
  ( contractId :: TxOutRef
  , transactionId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  | r
  )

type TxHeadersRow = TxRowBase (utxo :: Maybe TxOutRef)
newtype TxHeader = TxHeader { | TxHeadersRow }

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
  , txBody :: Maybe (TextEnvelope TransactionObject)
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
      , txBody: map (decodeMaybe decodeTransactionObjectTextEnvelope) :: Maybe _ -> Maybe _
      }

newtype ServerURL = ServerURL String

bech32ToParty :: Bech32 -> V1.Party
bech32ToParty bech32 = V1.Address (bech32ToString bech32)

partyToBech32 :: V1.Party -> Maybe Bech32
partyToBech32 (V1.Address str) = Just $ unsafeBech32 str
partyToBech32 _ = Nothing

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

-- | We perform GET and POST against this endpoint. Links structure is shared between response and resource.
newtype IndexEndpoint :: Type -> Type -> Row Type -> Type -> Row Type -> Type
newtype IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks =
  IndexEndpoint (ResourceLink (Array (ResourceWithLinks getResponse getResponseLinks)))

type IndexEndpoint' postRequest postResponse getResponse links =
  IndexEndpoint postRequest postResponse links getResponse links

derive instance Eq (IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks)
derive instance Newtype (IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks) _
derive newtype instance DecodeJson (IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks)

-- | We perform GET and PUT against this endpoint.
newtype ResourceEndpoint :: Type -> Type -> Row Type -> Type
newtype ResourceEndpoint putRequest getResponse links =
  ResourceEndpoint (ResourceLink (ResourceWithLinks getResponse links))

derive instance Eq (ResourceEndpoint putRequest getResponse links)
derive instance Newtype (ResourceEndpoint putRequest getResponse links) _
derive newtype instance DecodeJson (ResourceEndpoint putRequest getResponse links)

class ToResourceLink t a | t -> a where
  toResourceLink :: t -> ResourceLink a

instance ToResourceLink (ResourceLink a) a where
  toResourceLink = identity
else instance ToResourceLink (ResourceEndpoint putRequest getResponse links) (ResourceWithLinks getResponse links) where
  toResourceLink (ResourceEndpoint link) = toResourceLink link
else instance ToResourceLink (IndexEndpoint postRequest postResponse postResponsLinks getResponse getResponseLinks) (Array (ResourceWithLinks getResponse getResponseLinks)) where
  toResourceLink (IndexEndpoint link) = toResourceLink link
-- | I'm closing the type class here for convenience. If we want to have other instances we can drop this approach.
else instance (Newtype n t, ToResourceLink t a) => ToResourceLink n a where
  toResourceLink = toResourceLink <<< unwrap

decodeResourceWithLink
  :: forall a linksRow
   . DecodeRecord (resource :: DecodeJsonFieldFn a) (ResourceWithLinksRow a linksRow)
  => DecodeJsonFieldFn a
  -> Json
  -> Either JsonDecodeError (ResourceWithLinks a linksRow)
decodeResourceWithLink decodeResource = decodeRecord { resource: decodeResource }

class EncodeHeaders a r | a -> r where
  encodeHeaders :: Row.Homogeneous r String => a -> { | r }

class EncodeJsonBody a where
  encodeJsonBody :: a -> Json

-- API Endpoints
newtype PostMerkleizationRequest = PostMerkleizationRequest
  { contract :: V1.Contract
  }

instance EncodeJsonBody PostMerkleizationRequest where
  encodeJsonBody (PostMerkleizationRequest r) = encodeJson
    { contract: r.contract
    }

newtype PostMerkleizationResponse = PostMerkleizationResponse
  { contract :: V1.Contract
  , continuations :: Map String V1.Contract
  }

derive instance Newtype PostMerkleizationResponse _

derive newtype instance DecodeJson PostMerkleizationResponse

newtype PostContractsRequest = PostContractsRequest
  { metadata :: Metadata
  -- , version :: MarloweVersion
  -- , roles :: Maybe RolesConfig
  , contract :: V1.Contract
  , minUTxODeposit :: V1.Ada
  , changeAddress :: Bech32
  , addresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  }

instance EncodeJsonBody PostContractsRequest where
  encodeJsonBody (PostContractsRequest r) = encodeJson
    { metadata: r.metadata
    , version: V1
    , roles: Nothing :: Maybe String
    , contract: r.contract
    , minUTxODeposit: r.minUTxODeposit
    }

type PostContractsHeadersRow =
  ( "X-Change-Address" :: String
  , "X-Address" :: String
  -- , "X-Collateral-UTxO" :: String
  )

instance EncodeHeaders PostContractsRequest PostContractsHeadersRow where
  encodeHeaders (PostContractsRequest { changeAddress, addresses }) =
    { "X-Change-Address": bech32ToString changeAddress
    , "X-Address": String.joinWith "," (map bech32ToString addresses)
    -- FIXME: Empty collateral causes request rejection so ... this header record representation
    -- gonna be hard to maintain and we should switch to `Object String` probably and use
    -- lower level fetch API.
    -- , "X-Collateral-UTxO": String.joinWith "," (map txOutRefToString collateralUTxOs)
    }

-- FIXME: paluh. Change `txBody` to `tx` because we send(ing) on our branch the actual transaction and not
-- just the body.
newtype PostContractsResponseContent = PostContractsResponseContent
  { contractId :: TxOutRef
  , txBody :: TextEnvelope TransactionObject
  }

derive instance Newtype PostContractsResponseContent _

instance DecodeJson PostContractsResponseContent where
  decodeJson = decodeNewtypedRecord
    { txBody: map decodeTransactionObjectTextEnvelope :: Maybe _ -> Maybe _ }

type ContractEndpointRow r = ("contract" :: ContractEndpoint | r)

type TransactionsEndpointRow r = ("transactions" :: Maybe TransactionsEndpoint | r)

type PostContractsResponse = ResourceWithLinks PostContractsResponseContent (ContractEndpointRow + ())

type GetContractsResponseContent = ContractHeader

type GetContractsResponse = ResourceWithLinks GetContractsResponseContent (ContractEndpointRow + TransactionsEndpointRow + ())

newtype ContractsEndpoint = ContractsEndpoint
  ( IndexEndpoint PostContractsRequest PostContractsResponseContent (ContractEndpointRow + ()) GetContractsResponseContent
      (ContractEndpointRow + TransactionsEndpointRow + ())
  )

derive instance Eq ContractsEndpoint
derive instance Newtype ContractsEndpoint _
derive newtype instance DecodeJson ContractsEndpoint

newtype PutContractRequest = PutContractRequest (TextEnvelope TransactionWitnessSetObject)

instance EncodeHeaders PutContractRequest () where
  encodeHeaders (PutContractRequest _) = {}

instance EncodeJsonBody PutContractRequest where
  encodeJsonBody (PutContractRequest textEnvelope) = encodeJson textEnvelope

type GetContractResponse = ContractState

newtype ContractEndpoint = ContractEndpoint
  (ResourceEndpoint PutContractRequest GetContractResponse (transactions :: TransactionsEndpoint))

derive instance Eq ContractEndpoint
derive instance Newtype ContractEndpoint _
derive newtype instance DecodeJson ContractEndpoint

newtype PostTransactionsRequest = PostTransactionsRequest
  { inputs :: Array V1.Input
  , invalidBefore :: DateTime
  , invalidHereafter :: DateTime
  , metadata :: Metadata
  , changeAddress :: Bech32
  , addresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  }

instance EncodeJsonBody PostTransactionsRequest where
  encodeJsonBody (PostTransactionsRequest r) = encodeJson
    { inputs: r.inputs
    , invalidBefore: ISO r.invalidBefore
    , invalidHereafter: ISO r.invalidHereafter
    , metadata: r.metadata
    , version: V1
    }

instance EncodeHeaders PostTransactionsRequest PostContractsHeadersRow where
  encodeHeaders (PostTransactionsRequest { changeAddress, addresses }) = -- , collateralUTxOs }) =
    { "X-Change-Address": bech32ToString changeAddress
    , "X-Address": String.joinWith "," (map bech32ToString addresses)
    -- FIXME: Check comment above regarding the same header and contraacts endpoint request.
    -- , "X-Collateral-UTxO": String.joinWith "," (map txOutRefToString collateralUTxOs)
    }

newtype PostTransactionsResponse = PostTransactionsResponse
  { contractId :: TxOutRef
  , transactionId :: TxId
  , txBody :: TextEnvelope TransactionObject
  }

derive instance Newtype PostTransactionsResponse _

instance DecodeJson PostTransactionsResponse where
  decodeJson = decodeNewtypedRecord
    { txBody: map decodeTransactionObjectTextEnvelope :: Maybe _ -> Maybe _ }

type GetTransactionsResponse = TxHeader

newtype TransactionsEndpoint = TransactionsEndpoint
  (IndexEndpoint' PostTransactionsRequest PostTransactionsResponse GetTransactionsResponse (transaction :: TransactionEndpoint))

derive instance Eq TransactionsEndpoint
derive instance Newtype TransactionsEndpoint _
derive newtype instance DecodeJson TransactionsEndpoint

newtype PutTransactionRequest = PutTransactionRequest (TextEnvelope TransactionWitnessSetObject)

instance EncodeHeaders PutTransactionRequest () where
  encodeHeaders (PutTransactionRequest _) = {}

instance EncodeJsonBody PutTransactionRequest where
  encodeJsonBody (PutTransactionRequest textEnvelope) = encodeJson textEnvelope

type GetTransactionResponse = Tx

newtype TransactionEndpoint = TransactionEndpoint
  (ResourceEndpoint PutTransactionRequest GetTransactionResponse (previous :: Maybe TransactionEndpoint, next :: Maybe TransactionEndpoint))

derive instance Eq TransactionEndpoint
derive instance Newtype TransactionEndpoint _
derive newtype instance DecodeJson TransactionEndpoint

-- Entry point
api :: ContractsEndpoint
api = ContractsEndpoint (IndexEndpoint (ResourceLink "contracts"))

newtype Runtime = Runtime
  { root :: ContractsEndpoint
  , serverURL :: ServerURL
  }

runtime :: ServerURL -> Runtime
runtime serverURL = Runtime { root: api, serverURL }

