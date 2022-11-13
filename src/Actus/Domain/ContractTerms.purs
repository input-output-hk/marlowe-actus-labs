module Actus.Domain.ContractTerms where

import Prelude

import Data.Argonaut (JsonDecodeError(..), caseJsonString, encodeJson, fromString, stringify)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.DateTime (Date, DateTime)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)

-- |ContractType
data CT
  = PAM -- ^ Principal at maturity
  | LAM -- ^ Linear amortizer
  | NAM -- ^ Negative amortizer
  | ANN -- ^ Annuity

derive instance Generic CT _
derive instance Eq CT
derive instance Ord CT

instance EncodeJson CT where
  encodeJson a = genericEncodeJson a

instance DecodeJson CT where
  decodeJson a = genericDecodeJson a

-- |ContractRole
data CR
  = CR_RPA -- ^ Real position asset
  | CR_RPL -- ^ Real position liability
  | CR_CLO -- ^ Role of a collateral
  | CR_CNO -- ^ Role of a close-out-netting
  | CR_COL -- ^ Role of an underlying to a collateral
  | CR_LG -- ^ Long position
  | CR_ST -- ^ Short position
  | CR_BUY -- ^ Protection buyer
  | CR_SEL -- ^ Protection seller
  | CR_RFL -- ^ Receive first leg
  | CR_PFL -- ^ Pay first leg
  | CR_RF -- ^ Receive fix leg
  | CR_PF -- ^ Pay fix leg

instance EncodeJson CR where
  encodeJson CR_RPA = encodeJson "RPA"
  encodeJson CR_RPL = encodeJson "RPL"
  encodeJson CR_CLO = encodeJson "CLO"
  encodeJson CR_CNO = encodeJson "CNO"
  encodeJson CR_COL = encodeJson "COL"
  encodeJson CR_LG = encodeJson "LG"
  encodeJson CR_ST = encodeJson "ST"
  encodeJson CR_BUY = encodeJson "BUY"
  encodeJson CR_SEL = encodeJson "SEL"
  encodeJson CR_RFL = encodeJson "RFL"
  encodeJson CR_PFL = encodeJson "PFL"
  encodeJson CR_RF = encodeJson "RF"
  encodeJson CR_PF = encodeJson "PF"

instance DecodeJson CR where
  decodeJson json = caseJsonString
    (Left $ TypeMismatch $ "Unexpected json value: " <> stringify json)
    decode
    json
    where
    decode "RPL" = pure CR_RPL
    decode "CLO" = pure CR_CLO
    decode "CNO" = pure CR_CNO
    decode "COL" = pure CR_COL
    decode "LG" = pure CR_LG
    decode "ST" = pure CR_ST
    decode "BUY" = pure CR_BUY
    decode "SEL" = pure CR_SEL
    decode "RFL" = pure CR_RFL
    decode "PFL" = pure CR_PFL
    decode "RF" = pure CR_RF
    decode "PF" = pure CR_PF
    decode c = Left (TypeMismatch $ "Unexpected constructor name:" <> c)

-- |DayCountConvention
data DCC
  = DCC_A_AISDA -- ^ Actual/Actual ISDA
  | DCC_A_360 -- ^ Actual/360
  | DCC_A_365 -- ^ Actual/365
  | DCC_E30_360ISDA -- ^ 30E/360 ISDA
  | DCC_E30_360 -- ^ 30E/360
  | DCC_B_252 -- ^ Business / 252

derive instance Generic DCC _
derive instance Eq DCC

instance Show DCC where
  show DCC_A_AISDA = "AA"
  show DCC_A_360 = "A360"
  show DCC_A_365 = "A365"
  show DCC_E30_360ISDA = "30E360ISDA"
  show DCC_E30_360 = "30E360"
  show DCC_B_252 = "B252"

instance EncodeJson DCC where
  encodeJson DCC_A_AISDA = fromString "AA"
  encodeJson DCC_A_360 = fromString "A360"
  encodeJson DCC_A_365 = fromString "A365"
  encodeJson DCC_E30_360ISDA = fromString "30E360ISDA"
  encodeJson DCC_E30_360 = fromString "30E360"
  encodeJson DCC_B_252 = fromString "B252"

-- instance FromJSON DCC where
--   parseJSON (String "AA")         = return DCC_A_AISDA
--   parseJSON (String "A360")       = return DCC_A_360
--   parseJSON (String "A365")       = return DCC_A_365
--   parseJSON (String "30E360ISDA") = return DCC_E30_360ISDA
--   parseJSON (String "30E360")     = return DCC_E30_360
--   parseJSON (String "B252")       = return DCC_B_252
--   parseJSON _                     = mzero
--
-- -- |EndOfMonthConvention
data EOMC
  = EOMC_EOM -- ^ End of month
  | EOMC_SD -- ^ Same day

derive instance Generic EOMC _
derive instance Eq EOMC

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''EOMC)
--
-- -- |BusinessDayConvention
data BDC
  = BDC_NULL -- ^ No shift
  | BDC_SCF -- ^ Shift/calculate following
  | BDC_SCMF -- ^ Shift/calculate modified following
  | BDC_CSF -- ^ Calculate/shift following
  | BDC_CSMF -- ^ Calculate/shift modified following
  | BDC_SCP -- ^ Shift/calculate preceding
  | BDC_SCMP -- ^ Shift/calculate modified preceding
  | BDC_CSP -- ^ Calculate/shift preceding
  | BDC_CSMP -- ^ Calculate/shift modified preceding

derive instance Generic BDC _
derive instance Eq BDC

--          deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''BDC)
--
data Calendar
  = CLDR_MF -- ^ Monday to Friday
  | CLDR_NC -- ^ No calendar

--               deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''Calendar)

type ScheduleConfig =
  { calendar :: Maybe Calendar
  , endOfMonthConvention :: Maybe EOMC
  , businessDayConvention :: Maybe BDC
  }

--  decodeJson json = do
--    r <- { calendar: _, endoOfMonthConvention: _, bussinessDayConvention: _ }
--      <$> decodeJson (json :. "calendar")
--      <*> decodeJson ...
--      <*> decodeJson ...
--    pure $ ScheduleConfig r
--
--  decodeJson json = ado
--    r <- { calendar: _, endoOfMonthConvention: _, bussinessDayConvention: _ }
--    calendar <- decodeJson (json :. "calendar")
--    endOfMonthConvention <- decodeJson ...
--    businessDayConvention <- decodeJson ...
--    in $ ScheduleConfig
---     { calendar, endoOfMonthConvention, businessDayConvention }

--  deriving stock (Show, Generic)
--  deriving anyclass (FromJSON, ToJSON)

-- |ContractPerformance
data PRF
  = PRF_PF -- ^ Performant
  | PRF_DL -- ^ Delayed
  | PRF_DQ -- ^ Delinquent
  | PRF_DF -- ^ Default

--          deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''PRF)
--
-- |CreditEventTypeCovered
data CETC
  = CETC_DL -- ^ Delayed
  | CETC_DQ -- ^ Delinquent
  | CETC_DF -- ^ Default

--         deriving stock (Show, Read, Eq, Generic)

-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''CETC)
--
-- |GuaranteedExposure
data CEGE
  = CEGE_NO -- ^ Nominal value
  | CEGE_NI -- ^ Nominal value plus interest

--         deriving stock (Show, Read, Eq, Generic)

-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''CEGE)
--
-- |FeeBasis
data FEB
  = FEB_A -- ^ Absolute value
  | FEB_N -- ^ Notional of underlying

--         deriving stock (Show, Read, Eq, Generic)

-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''FEB)
--
-- |InterestCalculationBase
data IPCB
  = IPCB_NT -- ^ Calculation base always equals to NT
  | IPCB_NTIED -- ^ Notional remains constant amount as per IED
  | IPCB_NTL -- ^ Calculation base is notional base laged

derive instance Generic IPCB _
derive instance Eq IPCB
derive instance Ord IPCB

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''IPCB)
--
-- |ScalingEffect
data SCEF
  = SE_OOO -- ^ No scaling
  | SE_IOO -- ^ Only interest payments scaled
  | SE_ONO -- ^ Only nominal payments scaled
  | SE_OOM -- ^ Only maximum deferred amount scaled
  | SE_INO -- ^ Interest and nominal payments scaled
  | SE_ONM -- ^ Nominal and maximum deferred amount scaled
  | SE_IOM -- ^ Interest and maximum deferred amount scaled
  | SE_INM -- ^ Interest, nominal and maximum deferred amount scaled

derive instance Generic SCEF _
derive instance Eq SCEF

instance Show SCEF where
  show SE_OOO = "OOO"
  show SE_IOO = "IOO"
  show SE_ONO = "ONO"
  show SE_OOM = "OOM"
  show SE_INO = "INO"
  show SE_ONM = "ONM"
  show SE_IOM = "IOM"
  show SE_INM = "INM"

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''SCEF)
--
-- |PenaltyType
data PYTP
  = PYTP_A -- ^ Absolute
  | PYTP_N -- ^ Nominal rate
  | PYTP_I -- ^ Current interest rate differential
  | PYTP_O -- ^ No penalty

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''PYTP)
--
-- |Option Type
data OPTP
  = OPTP_C -- ^ Call Option
  | OPTP_P -- ^ Put Option
  | OPTP_CP -- ^ Call-Put Option

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''OPTP)
--
-- |Option Exercise Type
data OPXT
  = OPXT_E -- ^ European
  | OPXT_B -- ^ Bermudan
  | OPXT_A -- ^ American

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''OPXT)
--
-- |Settlement
data DS
  = DS_S -- ^ Cash Settlement
  | DS_D -- ^ Physical Settlement

--           deriving stock (Show, Read, Eq, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''DS)
--
-- |PrepaymentEffect
data PPEF
  = PPEF_N -- ^ No prepayment
  | PPEF_A -- ^ Prepayment allowed, prepayment results in reduction of PRNXT while MD remains
  | PPEF_M -- ^ Prepayment allowed, prepayment results in reduction of MD while PRNXT remains

--           deriving stock (Show, Read, Eq, Ord, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''PPEF)
--
data CalendarType
  = NoCalendar
  | MondayToFriday
  | CustomCalendar { holidays :: List Date }

--                   deriving stock (Show, Generic)
--                   deriving anyclass (FromJSON, ToJSON)
--
-- |CyclePeriod
data Period
  = P_D -- ^ Day
  | P_W -- ^ Week
  | P_M -- ^ Month
  | P_Q -- ^ Quarter
  | P_H -- ^ Half year
  | P_Y -- ^ Year

derive instance Generic Period _
derive instance Eq Period
derive instance Ord Period

--             deriving stock (Show, Read, Eq, Ord, Generic)
--
-- $(deriveJSON defaultOptions { constructorTagModifier = reverse . takeWhile (/= '_') . reverse } ''Period)
--
-- |CycleStub
data Stub
  = ShortStub -- ^ Short last stub
  | LongStub -- ^ Long last stub

--           deriving stock (Show, Eq, Ord, Generic)
--
-- instance ToJSON Stub where
--   toJSON ShortStub = String "1"
--   toJSON LongStub  = String "0"
--
-- instance FromJSON Stub where
--   parseJSON (String "1") = return ShortStub
--   parseJSON (String "0") = return LongStub
--   parseJSON _            = mzero
--
-- |Cycle
data Cycle = Cycle
  { n :: Int
  , p :: Period
  , stub :: Stub
  , includeEndDay :: Boolean
  }

--   deriving stock (Show, Eq, Ord, Generic)
--
-- instance ToJSON Cycle where
--   toJSON (Cycle n p s _) =
--     case toJSON p of
--       String p' ->
--         case toJSON s of
--           String s' ->
--             String $
--               'P'
--                 `cons` (pack $ show n)
--                 `append` p'
--                 `snoc` 'L'
--                 `append` s'
--           _ -> Null
--       _ -> Null
--
-- instance FromJSON Cycle where
--   parseJSON (String s) = fromMaybe mzero (parseCycle s)
--     where
--       parseCycle :: Text -> Maybe (Parser Cycle)
--       parseCycle c = do
--         r0 <- unconsConstant 'P' c
--         (n, r1) <- hush $ T.decimal r0
--         (p, r2) <- uncons r1
--         if T.null r2
--           then
--             Just $
--               return (Cycle n)
--                 <*> parseJSON (String $ singleton p)
--                 <*> return LongStub
--                 <*> return False
--           else do
--             r3 <- unconsConstant 'L' r2
--             Just $
--               return (Cycle n)
--                 <*> parseJSON (String $ singleton p)
--                 <*> parseJSON (String r3)
--                 <*> return False
--
--       unconsConstant :: Char -> Text -> Maybe Text
--       unconsConstant c t = do (ht, tt) <- uncons t
--                               guard (ht == c)
--                               return tt
--
--       hush :: Either a b -> Maybe b
--       hush = either (const Nothing) Just
--
--   parseJSON _ = mzero
--
-- -- For applicability failures
-- data TermValidationError =
--     Required String
--     | NotApplicable String
--     deriving stock (Eq)
-- instance Show TermValidationError where
--     show (Required s)      = "Missing required term: " ++ s
--     show (NotApplicable s) = "Term not applicable to contract: " ++ s
--
{-| ACTUS contract terms and attributes are defined in
    https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-terms.json
-}
data ContractTerms a = ContractTerms
  { -- General
    contractId :: String
  , contractType :: CT
  , contractRole :: CR
  , settlementCurrency :: Maybe String

  -- Calendar
  , initialExchangeDate :: Maybe DateTime -- ^ Initial Exchange Date
  , dayCountConvention :: Maybe DCC -- ^ Day Count Convention
  , scheduleConfig :: ScheduleConfig

  -- Contract Identification
  , statusDate :: DateTime -- ^ Status Date
  , marketObjectCodeRef :: Maybe String -- ^ Market Object Code

  -- Counterparty
  , contractPerformance :: Maybe PRF -- ^ Contract Performance
  , creditEventTypeCovered :: Maybe CETC -- ^ Credit Event Type Covered
  , coverageOfCreditEnhancement :: Maybe a -- ^ Coverage Of Credit Enhancement
  , guaranteedExposure :: Maybe CEGE -- ^ Guaranteed Exposure

  -- Fees
  , cycleOfFee :: Maybe Cycle -- ^ Cycle Of Fee
  , cycleAnchorDateOfFee :: Maybe DateTime -- ^ Cycle Anchor Date Of Fee
  , feeAccrued :: Maybe a -- ^ Fee Accrued
  , feeBasis :: Maybe FEB -- ^ Fee Basis
  , feeRate :: Maybe a -- ^ Fee Rate

  -- Interest
  , cycleAnchorDateOfInterestPayment :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Payment
  , cycleOfInterestPayment :: Maybe Cycle -- ^ Cycle Of Interest Payment
  , accruedInterest :: Maybe a -- ^ Accrued Interest
  , capitalizationEndDate :: Maybe DateTime -- ^ Capitalization End Date
  , cycleAnchorDateOfInterestCalculationBase :: Maybe DateTime -- ^ Cycle Anchor Date Of Interest Calculation Base
  , cycleOfInterestCalculationBase :: Maybe Cycle -- ^ Cycle Of Interest Calculation Base
  , interestCalculationBase :: Maybe IPCB -- ^ Interest Calculation Base
  , interestCalculationBaseA :: Maybe a -- ^ Interest Calculation Base Amount
  , nominalInterestRate :: Maybe a -- ^ Nominal Interest Rate
  , nominalInterestRate2 :: Maybe a -- ^ Nominal Interest Rate (Second Leg in Plain Vanilla Swap)
  , interestScalingMultiplier :: Maybe a -- ^ Interest Scaling Multiplier

  -- Dates
  , maturityDate :: Maybe DateTime -- ^ Maturity Date
  , amortizationDate :: Maybe DateTime -- ^ Amortization Date
  , exerciseDate :: Maybe DateTime -- ^ Exercise Date

  -- Notional Principal
  , notionalPrincipal :: Maybe a -- ^ Notional Principal
  , premiumDiscountAtIED :: Maybe a -- ^ Premium Discount At IED
  , cycleAnchorDateOfPrincipalRedemption :: Maybe DateTime -- ^ Cycle Anchor Date Of Principal Redemption
  , cycleOfPrincipalRedemption :: Maybe Cycle -- ^ Cycle Of Principal Redemption
  , nextPrincipalRedemptionPayment :: Maybe a -- ^ Next Principal Redemption Payment
  , purchaseDate :: Maybe DateTime -- ^ Purchase Date
  , priceAtPurchaseDate :: Maybe a -- ^ Price At Purchase Date
  , terminationDate :: Maybe DateTime -- ^ Termination Date
  , priceAtTerminationDate :: Maybe a -- ^ Price At Termination Date
  , quantity :: Maybe a -- ^ Quantity
  , currency :: Maybe String -- ^ The currency of the cash flows
  , currency2 :: Maybe String -- ^ The currency of the cash flows of the second leg

  -- Scaling Index
  , scalingIndexAtStatusDate :: Maybe a -- ^ Scaling Index At Status Date
  , cycleAnchorDateOfScalingIndex :: Maybe DateTime -- ^ Cycle Anchor Date Of Scaling Index
  , cycleOfScalingIndex :: Maybe Cycle -- ^ Cycle Of Scaling Index
  , scalingEffect :: Maybe SCEF -- ^ Scaling Effect
  , scalingIndexAtContractDealDate :: Maybe a -- ^ Scaling Index At Contract Deal Date
  , marketObjectCodeOfScalingIndex :: Maybe String -- ^ Market Object Code Of Scaling Index
  , notionalScalingMultiplier :: Maybe a -- ^ Notional Scaling Multiplier

  -- Optionality
  , cycleOfOptionality :: Maybe Cycle -- ^ Cycle Of Optionality
  , cycleAnchorDateOfOptionality :: Maybe DateTime -- ^ Cycle Anchor Date Of Optionality
  , optionType :: Maybe OPTP -- ^ Option Type
  , optionStrike1 :: Maybe a -- ^ Option Strike 1
  , optionExerciseType :: Maybe OPXT -- ^ Option Exercise Type

  -- Settlement
  , settlementPeriod :: Maybe Cycle -- ^ Settlement Period
  , deliverySettlement :: Maybe DS -- ^ Delivery Settlement
  , exerciseAmount :: Maybe a -- ^ Exercise Amount
  , futuresPrice :: Maybe a -- ^ Futures Price

  -- Penalty
  , penaltyRate :: Maybe a -- ^ Penalty Rate
  , penaltyType :: Maybe PYTP -- ^ Penalty Type
  , prepaymentEffect :: Maybe PPEF -- ^ Prepayment Effect

  -- Rate Reset
  , cycleOfRateReset :: Maybe Cycle -- ^ Cycle Of Rate Reset
  , cycleAnchorDateOfRateReset :: Maybe DateTime -- ^ Cycle Anchor Date Of Rate Reset
  , nextResetRate :: Maybe a -- ^ Next Reset Rate
  , rateSpread :: Maybe a -- ^ Rate Spread
  , rateMultiplier :: Maybe a -- ^ Rate Multiplier
  , periodFloor :: Maybe a -- ^ Period Floor
  , periodCap :: Maybe a -- ^ Period Cap
  , lifeCap :: Maybe a -- ^ Life Cap
  , lifeFloor :: Maybe a -- ^ Life Floor
  , marketObjectCodeOfRateReset :: Maybe String -- ^ Market Object Code Of Rate Reset

  -- Dividend
  , cycleOfDividend :: Maybe Cycle -- ^ Cycle Of Dividend
  , cycleAnchorDateOfDividend :: Maybe DateTime -- ^ Cycle Anchor Date Of Dividend
  , nextDividendPaymentAmount :: Maybe a -- ^ Next Dividend Payment Amount

  , enableSettlement :: Boolean -- ^ Enable settlement currency
  }
--   deriving stock (Show, Generic)
--   deriving anyclass (ToJSON)
--
-- instance FromJSON (Reference Double) where
--   parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }
--
-- instance FromJSON (ContractStructure Double) where
--   parseJSON (Object v) =
--     ContractStructure
--       <$> v .: "object"
--       <*> v .: "referenceType"
--       <*> v .: "referenceRole"
--   parseJSON _ = mzero
--
-- instance FromJSON (ContractTerms Double) where
--   parseJSON (Object v) =
--     ContractTerms
--       <$> (v .:  "contractID" <|> v .: "contractId")
--       <*> v .:  "contractType"
--       <*> (v .: "contractStructure" <|> return [])
--       <*> (v .:  "contractRole" <|> return CR_RPA) -- SWAPS tests miss contractRole in contract terms
--       <*> v .:? "settlementCurrency"
--       <*> v .:? "initialExchangeDate"
--       <*> v .:? "dayCountConvention"
--       <*> (v .: "scheduleConfig"
--            <|> ScheduleConfig
--                <$> (v .: "calendar" <|> return (Just CLDR_NC))
--                <*> (v .: "endOfMonthConvention" <|> return (Just EOMC_SD))
--                <*> (v .: "businessDayConvention" <|> return (Just BDC_NULL))
--            )
--       <*> v .:  "statusDate"
--       <*> v .:? "marketObjectCode"
--       <*> v .:? "contractPerformance"
--       <*> v .:? "creditEventTypeCovered"
--       <*> v .!? "coverageOfCreditEnhancement"
--       <*> v .!? "guaranteedExposure"
--       <*> v .:? "cycleOfFee"
--       <*> v .:? "cycleAnchorDateOfFee"
--       <*> v .:? "feeAccrued"
--       <*> v .:? "feeBasis"
--       <*> v .!? "feeRate"
--       <*> v .:? "cycleAnchorDateOfInterestPayment"
--       <*> v .:? "cycleOfInterestPayment"
--       <*> v .!? "accruedInterest"
--       <*> v .:? "capitalizationEndDate"
--       <*> v .:? "cycleAnchorDateOfInterestCalculationBase"
--       <*> v .:? "cycleOfInterestCalculationBase"
--       <*> v .:? "interestCalculationBase"
--       <*> v .!? "interestCalculationBaseAmount"
--       <*> v .!? "nominalInterestRate"
--       <*> v .!? "nominalInterestRate2"
--       <*> v .!? "interestScalingMultiplier"
--       <*> v .:? "maturityDate"
--       <*> v .:? "amortizationDate"
--       <*> v .:? "exerciseDate"
--       <*> v .!? "notionalPrincipal"
--       <*> v .!? "premiumDiscountAtIED"
--       <*> v .:? "cycleAnchorDateOfPrincipalRedemption"
--       <*> v .:? "cycleOfPrincipalRedemption"
--       <*> v .!? "nextPrincipalRedemptionPayment"
--       <*> v .:? "purchaseDate"
--       <*> v .!? "priceAtPurchaseDate"
--       <*> v .:? "terminationDate"
--       <*> v .!? "priceAtTerminationDate"
--       <*> v .!? "quantity"
--       <*> v .!? "currency"
--       <*> v .!? "currency2"
--       <*> v .:? "scalingIndexAtStatusDate"
--       <*> v .:? "cycleAnchorDateOfScalingIndex"
--       <*> v .:? "cycleOfScalingIndex"
--       <*> v .:? "scalingEffect"
--       <*> v .!? "scalingIndexAtContractDealDate"
--       <*> v .:? "marketObjectCodeOfScalingIndex"
--       <*> v .!? "notionalScalingMultiplier"
--       <*> v .:? "cycleOfOptionality"
--       <*> v .:? "cycleAnchorDateOfOptionality"
--       <*> v .:? "optionType"
--       <*> v .!? "optionStrike1"
--       <*> v .:? "optionExerciseType"
--       <*> v .:? "settlementPeriod"
--       <*> v .:? "deliverySettlement"
--       <*> v .!? "exerciseAmount"
--       <*> v .!? "futuresPrice"
--       <*> v .:? "penaltyRate"
--       <*> v .:? "penaltyType"
--       <*> v .:? "prepaymentEffect"
--       <*> v .:? "cycleOfRateReset"
--       <*> v .:? "cycleAnchorDateOfRateReset"
--       <*> v .!? "nextResetRate"
--       <*> v .!? "rateSpread"
--       <*> v .!? "rateMultiplier"
--       <*> v .:? "periodFloor"
--       <*> v .:? "periodCap"
--       <*> v .:? "lifeCap"
--       <*> v .:? "lifeFloor"
--       <*> v .:? "marketObjectCodeOfRateReset"
--       <*> v .:? "cycleOfDividendPayment"
--       <*> v .:? "cycleAnchorDateOfDividendPayment"
--       <*> v .:? "nextDividendPaymentAmount"
--       <*> (fromMaybe False <$> (v .:? "enableSettlement"))
--       <*> v .:? "constraints"
--     where
--       (.!?) w s = w .:? s <|> (fmap read <$> w .:? s)
--   parseJSON _ = mzero
