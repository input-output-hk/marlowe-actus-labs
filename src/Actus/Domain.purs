module Actus.Domain
  ( module Actus.Domain.BusinessEvents
  , module Actus.Domain.ContractState
  , module Actus.Domain.ContractTerms
  , module Actus.Domain.Schedule
  , class ActusFrac
  , _ceiling
  , _abs
  , _max
  , _min
  , class ActusOps
  , CashFlow(..)
  , RiskFactors(..)
  , setDefaultContractTermValues
  , sign
  ) where

import Actus.Domain.BusinessEvents (EventType(..))
import Actus.Domain.ContractState (ContractState(..))
import Actus.Domain.ContractTerms (BDC(..), CEGE(..), CETC(..), CR(..), CT(..), Calendar(..), ContractTerms(..), Cycle, DCC(..), DS(..), EOMC(..), FEB(..), IPCB(..), OPTP(..), OPXT(..), PPEF(..), PRF(..), PYTP(..), Period(..), SCEF(..), ScheduleConfig, Stub(..))
import Actus.Domain.Schedule (ShiftedDay, ShiftedSchedule, mkShiftedDay)
import Control.Alt ((<|>))
import Data.DateTime (DateTime)
import Data.Int (ceil)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Prelude (class Ring, max, min, negate, one, ($), (/))

class ActusOps a <= ActusFrac a where
  _ceiling :: a -> Int

class ActusOps a where
  _min :: a -> a -> a
  _max :: a -> a -> a
  _abs :: a -> a

instance ActusOps Number where
  _min = min
  _max = max
  _abs = abs

instance ActusFrac Number where
  _ceiling = ceil

-- | Risk factor observer
data RiskFactors a = RiskFactors
  { o_rf_CURS :: a
  , o_rf_RRMO :: a
  , o_rf_SCMO :: a
  , pp_payoff :: a
  , xd_payoff :: a
  , dv_payoff :: a
  }

-- deriving stock (Show, Generic)
-- deriving anyclass (FromJSON, ToJSON)

-- | Cash flows
data CashFlow a = CashFlow
  { tick :: Int
  , cashParty :: String
  , cashCounterParty :: String
  , cashPaymentDay :: DateTime
  , cashCalculationDay :: DateTime
  , cashEvent :: EventType
  , amount :: a
  , notional :: a
  , cashCurrency :: String
  }

-- deriving stock (Show, Eq, Generic)
-- deriving anyclass (FromJSON, ToJSON)

sign :: forall a. Ring a => CR -> a
sign CR_RPA = one
sign CR_RPL = negate one
sign CR_CLO = one
sign CR_CNO = one
sign CR_COL = one
sign CR_LG = one
sign CR_ST = negate one
sign CR_BUY = one
sign CR_SEL = negate one
sign CR_RFL = one
sign CR_PFL = negate one
sign CR_RF = one
sign CR_PF = negate one

-- == Default instance (Number)

setDefaultContractTermValues :: ContractTerms Number -> ContractTerms Number
setDefaultContractTermValues (ContractTerms ct) = ContractTerms $
  ct
    { scheduleConfig =
        { endOfMonthConvention: applyDefault EOMC_SD ct.scheduleConfig.endOfMonthConvention
        , businessDayConvention: applyDefault BDC_NULL ct.scheduleConfig.businessDayConvention
        , calendar: applyDefault CLDR_NC ct.scheduleConfig.calendar
        }
    , contractPerformance = applyDefault PRF_PF ct.contractPerformance
    , interestCalculationBase = applyDefault IPCB_NT ct.interestCalculationBase
    , premiumDiscountAtIED = applyDefault 0.0 ct.premiumDiscountAtIED
    , scalingEffect = applyDefault SE_OOO ct.scalingEffect
    , penaltyRate = applyDefault 0.0 ct.penaltyRate
    , penaltyType = applyDefault PYTP_O ct.penaltyType
    , prepaymentEffect = applyDefault PPEF_N ct.prepaymentEffect
    , rateSpread = applyDefault 0.0 ct.rateSpread
    , rateMultiplier = applyDefault 1.0 ct.rateMultiplier
    , feeAccrued = applyDefault 0.0 ct.feeAccrued
    , feeRate = applyDefault 0.0 ct.feeRate
    , accruedInterest = applyDefault 0.0 ct.accruedInterest
    , nominalInterestRate = applyDefault 0.0 ct.nominalInterestRate
    , priceAtPurchaseDate = applyDefault 0.0 ct.priceAtPurchaseDate
    , priceAtTerminationDate = applyDefault 0.0 ct.priceAtTerminationDate
    , scalingIndexAtContractDealDate = applyDefault 0.0 ct.scalingIndexAtContractDealDate
    , periodFloor = applyDefault (-infinity) ct.periodFloor
    , periodCap = applyDefault infinity ct.periodCap
    , lifeCap = applyDefault infinity ct.lifeCap
    , lifeFloor = applyDefault (-infinity) ct.lifeFloor
    }
  where
  infinity :: Number
  infinity = 1.0 / 0.0 :: Number

  applyDefault :: forall a. a -> Maybe a -> Maybe a
  applyDefault v o = o <|> Just v
