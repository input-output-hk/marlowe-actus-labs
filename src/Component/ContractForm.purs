module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
import Component.Modal (mkModal)
import Component.Modal as Modal
import Component.Types (MkComponentM, WalletInfo(..))
import Component.Widgets (link)
import Component.Widgets.Form (mkBooleanField)
import Contrib.React.Bootstrap.Form as Form
import Contrib.React.Bootstrap.Form.Label as Form.Label
import Control.Monad.Reader.Class (asks)
import Data.Argonaut (decodeJson, parseJson)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Debug (traceM)
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId(..), Party(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Language.Marlowe.Core.V1.Semantics.Types (Party(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (RiskFactorsMarlowe, genContract)
import React.Basic (JSX)
import React.Basic as DOOM
import React.Basic.DOM (css)
import React.Basic.DOM as DOOM
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.DOM.Simplified.Generated as DOM
import React.Basic.Events (EventHandler, handler, handler_)
import React.Basic.Hooks (type (/\), Hook, UseState, component, useEffect, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Wallet as Wallet

-- FIXME: We should add contractId to the callback params

type Result = ContractTerms Decimal /\ V1.Contract -- /\ ContractEndpoint)

type Props =
  { onSuccess :: Result -> Effect Unit
  , onError :: String -> Effect Unit
  , onDismiss :: Effect Unit
  , inModal :: Boolean
  , connectedWallet :: Maybe (WalletInfo Wallet.Api)
  }

mkContractForm :: MkComponentM (Props -> JSX)
mkContractForm = do
  booleanField <- liftEffect $ mkBooleanField
  runtime <- asks _.runtime
  modal <- liftEffect mkModal

  liftEffect $ component "ContractForm" \{ connectedWallet, onSuccess, onError, onDismiss, inModal } -> React.do
    possibleAddress /\ setPossibleAddress <- useState Nothing
    useEffectOnce $ do
      case connectedWallet of
        Nothing -> pure unit
        Just (WalletInfo { wallet }) -> launchAff_ do
          -- unused <- Wallet.getUsedAddresses
          -- used <- Wallet.getUsedAddresses
          traceM "GET CHANGE ADDRESS"
          address <- Wallet.getChangeAddress wallet
          liftEffect (setPossibleAddress $ const $ Just address)
      pure (pure unit)

    -- Let's hard code this for the testing phase.
    jsonString /\ onJsonStringChange <- useInput $ String.joinWith "\n"
      [ "{"
      , """  "contractType": "LAM","""
      , """  "contractID": "lam01","""
      , """  "contractRole": "RPA","""
      , """  "contractDealDate": "2012-12-28T00:00:00","""
      , """  "initialExchangeDate": "2013-01-01T00:00:00","""
      , """  "statusDate": "2012-12-30T00:00:00","""
      , """  "notionalPrincipal": " 5000","""
      , """  "cycleAnchorDateOfPrincipalRedemption": "2013-02-01T00:00:00","""
      , """  "nextPrincipalRedemptionPayment": " 500","""
      , """  "dayCountConvention": "A365","""
      , """  "nominalInterestRate": "0.08","""
      , """  "currency": "USD","""
      , """  "cycleOfPrincipalRedemption": "P1ML0","""
      , """  "cycleAnchorDateOfRateReset": "2013-04-01T00:00:00","""
      , """  "cycleOfRateReset": "P3ML1","""
      , """  "rateMultiplier": "1","""
      , """  "rateSpread": "0.1","""
      , """  "fixingDays": "P0D","""
      , """  "cycleAnchorDateOfInterestPayment": "2013-02-01T00:00:00","""
      , """  "cycleOfInterestPayment": "P1ML0","""
      , """  "endOfMonthConvention": "SD","""
      , """  "interestCalculationBase": "NT","""
      , """  "marketObjectCodeOfRateReset": "USD.SWP" """
      , "}"
      ]

    jsonValidation /\ setJsonValidation <- useState Nothing
    let
      renderJsonValidation =
        case jsonValidation of
          Nothing ->
            R.div { style: css { color: "black" }, children: [ R.text "Enter some JSON" ] }
          Just (Left err) ->
            R.div { style: css { color: "red" }, children: [ R.text $ show err ] }
          Just (Right _) ->
            R.div { style: css { color: "green" }, children: [ R.text "Valid JSON" ] }

      -- FIXME: plug in form validation
      validateForm = case parseJson jsonString >>= decodeJson of
        Right terms -> do

          let
            role1 = Role "R1" -- FIXME: provided as input
            role2 = Role "R2" -- FIXME: provided as input
            oracle = Address "" -- FIXME: oracle address
            -- party = Address 
            -- counterparty = 

            o_rf_CURS = fromMaybe (fromInt 1) $ do
              currency <- (unwrap terms).currency
              settlementCurrency <- (unwrap terms).settlementCurrency
              if currency == settlementCurrency then Nothing
              else Just $ ChoiceValue' (ChoiceId (currency <> settlementCurrency) oracle)
              where
              fromInt = Constant' <<< BigInt.fromInt <<< (marloweFixedPoint * _)

            riskFactors :: EventType -> DateTime -> RiskFactorsMarlowe
            riskFactors _ _ = RiskFactors
              { o_rf_CURS
              , o_rf_RRMO: ChoiceValue' (ChoiceId "rrmo" oracle) -- TODO: add to oracle
              , o_rf_SCMO: ChoiceValue' (ChoiceId "scmo" oracle) -- TODO: add to oracle
              , pp_payoff: ChoiceValue' (ChoiceId "pp" oracle) -- TODO: add to oracle
              }

            cashflowsMarlowe = genProjectedCashflows (role1 /\ role2) riskFactors terms
            contract = genContract cashflowsMarlowe

          setJsonValidation (const $ Just (Right contract))
          onSuccess (terms /\ contract)
        Left err -> setJsonValidation (const $ Just (Left err))

    pure $ do
      let
        formBody = DOM.div { className: "form-group" } do
          let
            mb3 = DOM.div { className: "mb-3" }
          [ mb3
              [ Form.label { htmlFor: "json" } [ R.text "Contract JSON" ]
              , R.textarea
                  { className: "form-control"
                  , id: "json"
                  , placeholder: "Please provide the contract JSON"
                  , value: jsonString
                  , onChange: onJsonStringChange
                  , rows: 15
                  }
              -- , renderJsonValidation
              ]
          , mb3
              [ DOOM.text $ show possibleAddress ]
          , mb3
              [ Form.label { htmlFor: "address" } [ R.text "Counterparty address" ]
              , Form.textInput
                  { id: "address"
                  , placeholder: "Enter a contract ID"
                  , value: "addr_test1qz4y0hs2kwmlpvwc6xtyq6m27xcd3rx5v95vf89q24a57ux5hr7g3tkp68p0g099tpuf3kyd5g80wwtyhr8klrcgmhasu26qcn"
                  }
              ]

          -- , booleanField
          --     { initialValue: false
          --     , onToggle: (const $ pure unit)
          --     , disabled: false
          --     , label: (R.text "Pick unused address")
          --     }
          ]
        formActions = DOOM.fragment
          [ link
              { label: DOOM.text "Cancel"
              , onClick: onDismiss
              , showBorders: true
              }
          , DOM.button
              { className: "btn btn-primary"
              , onClick: handler_ validateForm
              }
              [ R.text "Submit" ]
          ]

      if inModal then modal
        { title: R.text "Add contract"
        , onDismiss
        , body: formBody
        , footer: formActions
        , size: Modal.Large
        }
      else
        formBody

useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

