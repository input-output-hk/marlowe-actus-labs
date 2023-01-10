module CardanoMultiplatformLib.Transaction where

import Prelude

import CardanoMultiplatformLib.Types (JsonString)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Newtype (class Newtype)
import Effect (Effect)
import JS.Object (EffectMth0, EffectMth1, EffectMth2, JSObject)
import JS.Object.Generic (mkNewtypedFFI)
import Type.Prelude (Proxy(..))

newtype Transaction = Transaction
  ( JSObject
      ( from_bytes :: EffectMth1 Uint8Array TransactionObject
      , from_json :: EffectMth1 JsonString TransactionObject
      , new :: EffectMth2 TransactionBodyObject TransactionWitnessSetObject TransactionObject
      )
  )

derive instance Newtype Transaction _

transaction
  :: { from_bytes :: Transaction -> Uint8Array -> Effect TransactionObject
     , from_json :: Transaction -> JsonString -> Effect TransactionObject
     , new :: Transaction -> TransactionBodyObject -> TransactionWitnessSetObject -> Effect TransactionObject
     }
transaction = mkNewtypedFFI (Proxy :: Proxy Transaction)

newtype TransactionObject = TransactionObject
  ( JSObject
      ( free :: EffectMth0 Unit
      , to_bytes :: EffectMth0 Uint8Array
      , to_json :: EffectMth0 JsonString
      )
  )

derive instance Newtype TransactionObject _

transactionBodyObject :: { free :: TransactionBodyObject -> Effect Unit }
transactionBodyObject = mkNewtypedFFI (Proxy :: Proxy TransactionBodyObject)

-- export class TransactionBody {
--   free(): void;
--   to_bytes(): Uint8Array;
--
--   static from_bytes(bytes: Uint8Array): TransactionBody;
--
--   to_js_value(): TransactionBodyJSON;
--
--   static from_json(json: string): TransactionBody;
--
--   inputs(): TransactionInputs;
--
--   outputs(): TransactionOutputs;
--
--   fee(): BigNum;
--
--   ttl(): BigNum | undefined;
--
--   set_certs(certs: Certificates): void;
--
--   certs(): Certificates | undefined;
--
--   set_withdrawals(withdrawals: Withdrawals): void;
--
--   withdrawals(): Withdrawals | undefined;
--
--   set_update(update: Update): void;
--
--   update(): Update | undefined;
--
--   set_auxiliary_data_hash(auxiliary_data_hash: AuxiliaryDataHash): void;
--
--   auxiliary_data_hash(): AuxiliaryDataHash | undefined;
--
--   set_validity_start_interval(validity_start_interval: BigNum): void;
--
--   validity_start_interval(): BigNum | undefined;
--
--   set_mint(mint: Mint): void;
--
--   mint(): Mint | undefined;
--
--   multiassets(): Mint | undefined;
--
--   set_script_data_hash(script_data_hash: ScriptDataHash): void;
--
--   script_data_hash(): ScriptDataHash | undefined;
--
--   set_collateral(collateral: TransactionInputs): void;
--
--   collateral(): TransactionInputs | undefined;
--
--   set_required_signers(required_signers: Ed25519KeyHashes): void;
--
--   required_signers(): Ed25519KeyHashes | undefined;
--
--   set_network_id(network_id: NetworkId): void;
--
--   network_id(): NetworkId | undefined;
--
--   set_collateral_return(collateral_return: TransactionOutput): void;
--
--   collateral_return(): TransactionOutput | undefined;
--
--   set_total_collateral(total_collateral: BigNum): void;
--
--   total_collateral(): BigNum | undefined;
--
--   set_reference_inputs(reference_inputs: TransactionInputs): void;
--
--   reference_inputs(): TransactionInputs | undefined;
--
--   static new(inputs: TransactionInputs, outputs: TransactionOutputs, fee: BigNum, ttl?: BigNum): TransactionBody;
-- }

newtype TransactionBody = TransactionBody
  ( JSObject
      ( from_bytes :: EffectMth1 Uint8Array TransactionBodyObject
      , from_json :: EffectMth1 JsonString TransactionBodyObject
      )
  )

derive instance Newtype TransactionBody _

transactionBody
  :: { from_bytes :: TransactionBody -> Uint8Array -> Effect TransactionBodyObject
     , from_json :: TransactionBody -> JsonString -> Effect TransactionBodyObject
     }
transactionBody = mkNewtypedFFI (Proxy :: Proxy TransactionBody)

newtype TransactionBodyObject = TransactionBodyObject (JSObject (free :: EffectMth0 Unit))

derive instance Newtype TransactionBodyObject _

transactionObject
  :: { free :: TransactionObject -> Effect Unit
     , to_bytes :: TransactionObject -> Effect Uint8Array
     , to_json :: TransactionObject -> Effect JsonString
     }
transactionObject = mkNewtypedFFI (Proxy :: Proxy TransactionObject)

-- export class TransactionWitnessSet {
--   free(): void;
-- /**
-- * @returns {Uint8Array}
-- */
--   to_bytes(): Uint8Array;
-- /**
-- * @param {Uint8Array} bytes
-- * @returns {TransactionWitnessSet}
-- */
--   static from_bytes(bytes: Uint8Array): TransactionWitnessSet;
-- /**
-- * @returns {string}
-- */
--   to_json(): string;
-- /**
-- * @returns {TransactionWitnessSetJSON}
-- */
--   to_js_value(): TransactionWitnessSetJSON;
-- /**
-- * @param {string} json
-- * @returns {TransactionWitnessSet}
-- */
--   static from_json(json: string): TransactionWitnessSet;
-- /**
-- * @param {Vkeywitnesses} vkeys
-- */
--   set_vkeys(vkeys: Vkeywitnesses): void;
-- /**
-- * @returns {Vkeywitnesses | undefined}
-- */
--   vkeys(): Vkeywitnesses | undefined;
-- /**
-- * @param {NativeScripts} native_scripts
-- */
--   set_native_scripts(native_scripts: NativeScripts): void;
-- /**
-- * @returns {NativeScripts | undefined}
-- */
--   native_scripts(): NativeScripts | undefined;
-- /**
-- * @param {BootstrapWitnesses} bootstraps
-- */
--   set_bootstraps(bootstraps: BootstrapWitnesses): void;
-- /**
-- * @returns {BootstrapWitnesses | undefined}
-- */
--   bootstraps(): BootstrapWitnesses | undefined;
-- /**
-- * @param {PlutusV1Scripts} plutus_v1_scripts
-- */
--   set_plutus_v1_scripts(plutus_v1_scripts: PlutusV1Scripts): void;
-- /**
-- * @returns {PlutusV1Scripts | undefined}
-- */
--   plutus_v1_scripts(): PlutusV1Scripts | undefined;
-- /**
-- * @param {PlutusList} plutus_data
-- */
--   set_plutus_data(plutus_data: PlutusList): void;
-- /**
-- * @returns {PlutusList | undefined}
-- */
--   plutus_data(): PlutusList | undefined;
-- /**
-- * @param {Redeemers} redeemers
-- */
--   set_redeemers(redeemers: Redeemers): void;
-- /**
-- * @returns {Redeemers | undefined}
-- */
--   redeemers(): Redeemers | undefined;
-- /**
-- * @param {PlutusV2Scripts} plutus_v2_scripts
-- */
--   set_plutus_v2_scripts(plutus_v2_scripts: PlutusV2Scripts): void;
-- /**
-- * @returns {PlutusV2Scripts | undefined}
-- */
--   plutus_v2_scripts(): PlutusV2Scripts | undefined;
-- /**
-- * @returns {TransactionWitnessSet}
-- */
--   static new(): TransactionWitnessSet;
-- }

newtype TransactionWitnessSet = TransactionWitnessSet
  ( JSObject
      ( from_bytes :: EffectMth1 Uint8Array TransactionWitnessSetObject
      , from_json :: EffectMth1 JsonString TransactionWitnessSetObject
      )
  )

derive instance Newtype TransactionWitnessSet _

transactionWitness
  :: { from_bytes :: TransactionWitnessSet -> Uint8Array -> Effect TransactionWitnessSetObject
     , from_json :: TransactionWitnessSet -> JsonString -> Effect TransactionWitnessSetObject
     }
transactionWitness = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSet)

newtype TransactionWitnessSetObject = TransactionWitnessSetObject (JSObject (free :: EffectMth0 Unit))

derive instance Newtype TransactionWitnessSetObject _

transactionWitnessObject
  :: { free :: TransactionWitnessSetObject -> Effect Unit
     }
transactionWitnessObject = mkNewtypedFFI (Proxy :: Proxy TransactionWitnessSetObject)
