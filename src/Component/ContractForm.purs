module Component.ContractForm where

import Prelude

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ContractTerms, EventType, RiskFactors(..), Value'(..), marloweFixedPoint)
import Component.Widgets.Form (mkBooleanField)
import Data.Argonaut (decodeJson, parseJson)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Language.Marlowe.Core.V1.Semantics.Types (Party(..))
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (RiskFactorsMarlowe, genContract, toMarlowe)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (type (/\), Hook, UseState, component, useState, (/\))
import React.Basic.Hooks as React

mkContractForm :: Effect ((ContractTerms Decimal /\ V1.Contract -> Effect Unit) -> JSX)
mkContractForm = do
  booleanField <- mkBooleanField

  component "Form" \onNewContract -> React.do
    -- Let's hard code this for testing phase.
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
            termsMarlowe = toMarlowe terms

            role1 = Role "R1" -- FIXME
            role2 = Role "R2" -- FIXME

            riskFactors :: EventType -> DateTime -> RiskFactorsMarlowe
            riskFactors _ _ = RiskFactors -- FIXME
              { o_rf_CURS: fromInt 1
              , o_rf_RRMO: fromInt 1
              , o_rf_SCMO: fromInt 1
              , pp_payoff: fromInt 0
              }
              where
              fromInt = Constant' <<< BigInt.fromInt <<< (marloweFixedPoint * _)

            cashflowsMarlowe = genProjectedCashflows (role1 /\ role2) riskFactors termsMarlowe
            contract = genContract cashflowsMarlowe
          setJsonValidation (const $ Just (Right contract))
          onNewContract (terms /\ contract)
        Left err -> setJsonValidation (const $ Just (Left err))
    pure $
      R.div
        { className: "form-group"
        , children:
            [ R.textarea
                { className: "form-control"
                , placeholder: "Enter some JSON"
                , value: jsonString
                , onChange: onJsonStringChange
                , rows: 15
                }
            , renderJsonValidation
            , booleanField
                { initialValue: false
                , onToggle: (const $ pure unit)
                , disabled: false
                , label: (R.text "Pick unused address")
                }
            ]
        }

useInput :: String -> Hook (UseState String) (String /\ EventHandler)
useInput initialValue = React.do
  value /\ setValue <- useState initialValue
  let onChange = handler targetValue (setValue <<< const <<< fromMaybe "")
  pure (value /\ onChange)

