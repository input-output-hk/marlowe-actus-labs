module Marlowe.Actus.Metadata where

import Prelude

import Actus.Domain.ContractTerms (ContractTerms(..), encodeCycle)
import Data.Argonaut (Json, encodeJson, fromString, jsonEmptyObject, (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Decimal (Decimal, toString)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)

newtype Metadata value = Metadata (ContractTerms value)
derive instance Generic (Metadata value) _

instance EncodeJson (Metadata Decimal) where
  encodeJson (Metadata (ContractTerms ct)) =
       "cid" := ct.contractId
    ~> "ct" := ct.contractType
    ~> "cntrl" := ct.contractRole
    ~> "curs" :=? ct.settlementCurrency
    ~>? "ied" :=? (encodeDateTime <$> ct.initialExchangeDate)
    ~>? "dcc" :=? ct.dayCountConvention
    ~>? "sc" := ct.scheduleConfig 
    ~> "sd" := (encodeDateTime ct.statusDate)
    ~> "moc" :=? ct.marketObjectCode
    ~>? "prf" :=? ct.contractPerformance 
    ~>? "cetc" :=? ct.creditEventTypeCovered 
    ~>? "cecv" :=? (encodeDecimal <$> ct.coverageOfCreditEnhancement)
    ~>? "cege" :=? ct.guaranteedExposure 
    ~>? "fecl" :=? (encodeCycle <$> ct.cycleOfFee)
    ~>? "feanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfFee)
    ~>? "feac" :=? (encodeDecimal <$> ct.feeAccrued)
    ~>? "feb" :=? ct.feeBasis
    ~>? "fer" :=? (encodeDecimal <$> ct.feeRate)
    ~>? "ipanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfInterestPayment)
    ~>? "ipcl" :=? (encodeCycle <$> ct.cycleOfInterestPayment)
    ~>? "ipac" :=? (encodeDecimal <$> ct.accruedInterest)
    ~>? "ipced" :=? (encodeDateTime <$> ct.capitalizationEndDate)
    ~>? "ipcbanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfInterestCalculationBase)
    ~>? "ipcbcl" :=? (encodeCycle <$> ct.cycleOfInterestCalculationBase)
    ~>? "ipcb" :=? ct.interestCalculationBase 
    ~>? "ipcba" :=? (encodeDecimal <$> ct.interestCalculationBaseAmount)
    ~>? "ipnr" :=? (encodeDecimal <$> ct.nominalInterestRate)
    ~>? "ipnr2" :=? (encodeDecimal <$> ct.nominalInterestRate2) 
    ~>? "scip" :=? (encodeDecimal <$> ct.interestScalingMultiplier) 
    ~>? "md" :=? (encodeDateTime <$> ct.maturityDate)
    ~>? "ad" :=? (encodeDateTime <$> ct.amortizationDate)
    ~>? "xd" :=? (encodeDateTime <$> ct.exerciseDate)
    ~>? "nt" :=? (encodeDecimal <$> ct.notionalPrincipal)
    ~>? "pdied" :=? (encodeDecimal <$> ct.premiumDiscountAtIED)
    ~>? "pranx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfPrincipalRedemption)
    ~>? "prcl" :=? (encodeCycle <$> ct.cycleOfPrincipalRedemption)
    ~>? "prnxt" :=? (encodeDecimal <$> ct.nextPrincipalRedemptionPayment)
    ~>? "prd" :=? (encodeDateTime <$> ct.purchaseDate)
    ~>? "pprd" :=? (encodeDecimal <$> ct.priceAtPurchaseDate) 
    ~>? "td" :=? (encodeDateTime <$> ct.terminationDate)
    ~>? "ptd" :=? (encodeDecimal <$> ct.priceAtTerminationDate)
    ~>? "qt" :=? (encodeDecimal <$> ct.quantity)
    ~>? "cur" :=? ct.currency
    ~>? "cur2" :=? ct.currency2
    ~>? "scsd" :=? (encodeDecimal <$> ct.scalingIndexAtStatusDate)
    ~>? "scanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfScalingIndex)
    ~>? "sccl" :=? (encodeCycle <$> ct.cycleOfScalingIndex)
    ~>? "scef" :=? ct.scalingEffect
    ~>? "sccdd" :=? (encodeDecimal <$> ct.scalingIndexAtContractDealDate)
    ~>? "scmo" :=? ct.marketObjectCodeOfScalingIndex 
    ~>? "scnt" :=? (encodeDecimal <$> ct.notionalScalingMultiplier)
    ~>? "opcl" :=? (encodeCycle <$> ct.cycleOfOptionality)
    ~>? "opanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfOptionality)
    ~>? "optp" :=? ct.optionType 
    ~>? "ops1" :=? (encodeDecimal <$> ct.optionStrike1)
    ~>? "opxt" :=? ct.optionExerciseType 
    ~>? "stp" :=? (encodeCycle <$> ct.settlementPeriod)
    ~>? "ds" :=? ct.deliverySettlement 
    ~>? "xa" :=? (encodeDecimal <$> ct.exerciseAmount)
    ~>? "pfut" :=? (encodeDecimal <$> ct.futuresPrice)
    ~>? "pyrt" :=? (encodeDecimal <$> ct.penaltyRate)
    ~>? "pytp" :=? ct.penaltyType 
    ~>? "ppef" :=? ct.prepaymentEffect 
    ~>? "rrcl" :=? (encodeCycle <$> ct.cycleOfRateReset)
    ~>? "rranx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfRateReset)
    ~>? "rrnxt" :=? (encodeDecimal <$> ct.nextResetRate)
    ~>? "rrsp" :=? (encodeDecimal <$> ct.rateSpread)
    ~>? "rrmlt" :=? (encodeDecimal <$> ct.rateMultiplier)
    ~>? "rrpf" :=? (encodeDecimal <$> ct.periodFloor)
    ~>? "rrpc" :=? (encodeDecimal <$> ct.periodCap)
    ~>? "rrlc" :=? (encodeDecimal <$> ct.lifeCap)
    ~>? "rrlf" :=? (encodeDecimal <$> ct.lifeFloor)
    ~>? "rrmo" :=? ct.marketObjectCodeOfRateReset
    ~>? "dvcl" :=? (encodeCycle <$> ct.cycleOfDividendPayment)
    ~>? "dvanx" :=? (encodeDateTime <$> ct.cycleAnchorDateOfDividendPayment)
    ~>? "dvnxt" :=? (encodeDecimal <$> ct.nextDividendPaymentAmount)
    ~>? jsonEmptyObject

encodeDecimal :: Decimal -> Json
encodeDecimal = fromString <<< toString

encodeDateTime :: DateTime -> Json
encodeDateTime = encodeJson <<< unwrap <<< unInstant <<< fromDateTime
