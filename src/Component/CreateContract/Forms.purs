module Component.CreateContract.Forms where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms)
import CardanoMultiplatformLib (Bech32, bech32FromBytes, bech32FromString, bech32ToString, runGarbageCollector)
import CardanoMultiplatformLib as CardanoMultiplatformLib
import CardanoMultiplatformLib.Types (cborHexToCbor)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Contrib.React.Basic.Hooks.UseForm (useForm)
import Contrib.React.Basic.Hooks.UseForm as UseForm
import Contrib.React.Bootstrap.FormBuilder (FormBuilder, BootstrapForm)
import Contrib.React.Bootstrap.FormBuilder as FormBuilder
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.Array (elem, fromFoldable)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (fromInt, toNumber, toString)
import Data.Either (Either(..), hush)
import Data.FormURLEncoded.Query (FieldId(..), Query)
import Data.Formatter.DateTime (formatDateTime)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Time.Duration (Seconds(..))
import Data.Undefined.NoProblem as NoProblem
import Data.Validation.Semigroup (V(..))
import Debug (traceM)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party, Value(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows, currenciesWith6Decimals, defaultRiskFactors, evalVal, genContract, toMarloweCashflow)
import Marlowe.Runtime.Web.Types (TxOutRef, bech32ToParty)
import Polyform.Batteries (rawError)
import Polyform.Batteries as Batteries
import Polyform.Validator (liftFnEither, liftFnMMaybe) as Validator
import React.Basic (JSX)
import React.Basic (fragment) as DOOM
import React.Basic.DOM (text) as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Hooks (component, useEffectOnce, useState', (/\))
import React.Basic.Hooks as React
import Wallet as Wallet
import WalletContext (walletAddresses)

type FormSpec m = UseForm.Form m Unit -- JSX

type Result =
  { contractTerms :: ContractTerms
  , cashFlows :: CashFlows
  , contract :: V1.Contract
  , counterParty :: V1.Party
  , party :: V1.Party

  , changeAddress :: Bech32
  , usedAddresses :: Array Bech32
  , collateralUTxOs :: Array TxOutRef
  }


addressInput :: CardanoMultiplatformLib.Lib -> String -> String -> Maybe String -> FormBuilder Effect Bech32
addressInput cardanoMultiplatformLib label initial name = do
  let
    props =
      { initial
      , label: Just $ DOOM.text label
      , name
      , validator: Validator.liftFnMMaybe (const $ pure [ "Invalid address" ]) \str -> do
          bech32FromString cardanoMultiplatformLib str
      }
  FormBuilder.textInput props

initialAddress :: String
initialAddress = "" -- "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"

initialJson :: String
initialJson = String.joinWith "\n"
  [ "{"
  , """ "contractType": "PAM", """
  , """ "contractID": "pam01", """
  , """ "statusDate": "2023-12-31T00:00:00", """
  , """ "contractDealDate": "2023-12-28T00:00:00", """
  , """ "currency": "DjedTestUSD", """
  , """ "notionalPrincipal": "1000", """
  , """ "initialExchangeDate": "2024-01-01T00:00:00", """
  , """ "maturityDate": "2025-01-01T00:00:00", """
  , """ "nominalInterestRate": "0.1", """
  , """ "cycleAnchorDateOfInterestPayment": "2025-01-01T00:00:00", """
  , """ "cycleOfInterestPayment": "P1YL0", """
  , """ "dayCountConvention": "30E360", """
  , """ "endOfMonthConvention": "SD", """
  , """ "premiumDiscountAtIED": "   0", """
  , """ "rateMultiplier": "1.0", """
  , """ "contractRole": "RPA" """
  , "}"
  ]


mkForm :: CardanoMultiplatformLib.Lib -> BootstrapForm Effect Query Result
mkForm cardanoMultiplatformLib = FormBuilder.evalBuilder ado
  contractTerms <- FormBuilder.textArea
    { missingError: "Please provide contract terms JSON value"
    , initial: initialJson
    , validator: Validator.liftFnEither \jsonString -> do
        json <- lmap (const $ [ "Invalid JSON" ]) $ parseJson jsonString
        lmap (Array.singleton <<< show) (decodeJson json)
    , rows: 15
    , name: (Just $ "contract-terms")
    }
  partyAddress <- addressInput cardanoMultiplatformLib "Your address" "" $ Just "party"
  counterPartyAddress <- addressInput cardanoMultiplatformLib "Counter-party address" initialAddress $ Just "counter-party"
  let
    counterParty = bech32ToParty counterPartyAddress
    party = bech32ToParty partyAddress
    cashFlows =
      genProjectedCashflows
        (party /\ counterParty)
        (defaultRiskFactors contractTerms)
        contractTerms
    contract = genContract cashFlows
  in
    { contractTerms
    , cashFlows
    , contract
    , counterParty
    , party

    -- Ugly hack
    , usedAddresses: []
    , changeAddress: partyAddress
    , collateralUTxOs: []
    }

