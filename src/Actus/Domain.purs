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
  , marloweFixedPoint
  , Value'(..)
  , Observation'(..)
  ) where

import Prelude

import Actus.Domain.BusinessEvents (EventType(..))
import Actus.Domain.ContractState (ContractState(..))
import Actus.Domain.ContractTerms (BDC(..), CEGE(..), CETC(..), CR(..), CT(..), Calendar(..), ContractTerms(..), Cycle, DCC(..), DS(..), EOMC(..), FEB(..), IPCB(..), OPTP(..), OPXT(..), PPEF(..), PRF(..), PYTP(..), Period(..), SCEF(..), ScheduleConfig, Stub(..))
import Actus.Domain.Schedule (ShiftedDay, ShiftedSchedule, mkShiftedDay)
import Control.Alt ((<|>))
import Data.BigInt.Argonaut (BigInt, abs, quot, rem)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.Decimal (Decimal, ceil, fromNumber, toNumber)
import Data.Decimal as Decimal
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (signum)
import Data.Show.Generic (genericShow)

class ActusOps a <= ActusFrac a where
  _ceiling :: a -> Int

class ActusOps a where
  _min :: a -> a -> a
  _max :: a -> a -> a
  _abs :: a -> a

instance ActusOps Decimal where
  _min = min
  _max = max
  _abs = Decimal.abs

instance ActusFrac Decimal where
  _ceiling = Int.ceil <<< toNumber <<< ceil

data Value'
  = Constant' BigInt
  | NegValue' Value'
  | AddValue' Value' Value'
  | SubValue' Value' Value'
  | MulValue' Value' Value'
  | Cond' Observation' Value' Value'

data Observation'
  = AndObs' Observation' Observation'
  | OrObs' Observation' Observation'
  | NotObs' Observation'
  | ValueGE' Value' Value'
  | ValueGT' Value' Value'
  | ValueLT' Value' Value'
  | ValueLE' Value' Value'
  | ValueEQ' Value' Value'
  | TrueObs'
  | FalseObs'

instance Semiring Value' where
  add x y = AddValue' x y
  mul x y = division (MulValue' x y) (Constant' $ BigInt.fromInt marloweFixedPoint)
  one = Constant' $ BigInt.fromInt marloweFixedPoint
  zero = Constant' (BigInt.fromInt 0)

instance Ring Value' where
  sub x y = SubValue' x y

instance CommutativeRing Value'

instance EuclideanRing Value' where
  degree _ = 1
  div = division -- different rounding, not using DivValue
  mod x y = Constant' $ evalVal x `mod` evalVal y

instance ActusOps Value' where
  _min x y = Cond' (ValueLT' x y) x y
  _max x y = Cond' (ValueGT' x y) x y
  _abs a = _max a (NegValue' a)
    where
    _max x y = Cond' (ValueGT' x y) x y

instance ActusFrac Value' where
  _ceiling _ = 0 -- FIXME

derive instance Generic Value' _
derive instance Generic Observation' _

instance Show Value' where
  show (Constant' i) = "Constant " <> show i
  show (NegValue' v) = "-" <> show v
  show (AddValue' a b) = show a <> "+" <> show b
  show (SubValue' a b) = show a <> "-" <> show b
  show (MulValue' a b) = show a <> "*" <> show b
  show (Cond' o a b) = show "if ( " <> show o <> ") then " <> show a <> " else " <> show b

instance Show Observation' where
  show (AndObs' a b) = "AndObs (" <> show a <> "," <> show b <> ")"
  show (OrObs' a b) = "OrObs (" <> show a <> "," <> show b <> ")"
  show (NotObs' a) = "NotObs " <> show a
  show (ValueGE' a b) = "ValueGE (" <> show a <> "," <> show b <> ")"
  show (ValueGT' a b) = "ValueGT (" <> show a <> "," <> show b <> ")"
  show (ValueLT' a b) = "ValueLT (" <> show a <> "," <> show b <> ")"
  show (ValueLE' a b) = "ValueLE (" <> show a <> "," <> show b <> ")"
  show (ValueEQ' a b) = "ValueEQ (" <> show a <> "," <> show b <> ")"
  show TrueObs' = "TrueObs"
  show FalseObs' = "FalseObs"

marloweFixedPoint :: Int
marloweFixedPoint = 1000000

division :: Value' -> Value' -> Value'
division lhs rhs =
  do
    let
      n = evalVal lhs
      d = evalVal rhs
    Constant' (division' n d)
  where
  division' :: BigInt -> BigInt -> BigInt
  division' x _ | x == BigInt.fromInt 0 = BigInt.fromInt 0
  division' _ y | y == BigInt.fromInt 0 = BigInt.fromInt 0
  division' n d =
    let
      q = n `quot` d
      r = n `rem` d
      ar = abs r * (BigInt.fromInt 2)
      ad = abs d
    in
      if ar < ad then q -- reminder < 1/2
      else if ar > ad then q + signum n * signum d -- reminder > 1/2
      else
        let -- reminder == 1/2
          qIsEven = q `rem` (BigInt.fromInt 2) == (BigInt.fromInt 0)
        in
          if qIsEven then q else q + signum n * signum d

evalVal :: Value' -> BigInt
evalVal (Constant' n) = n
evalVal (NegValue' n) = -evalVal n
evalVal (AddValue' a b) = (evalVal a) + (evalVal b)
evalVal (SubValue' a b) = (evalVal a) - (evalVal b)
evalVal (MulValue' a b) = (evalVal a) * (evalVal b)
evalVal (Cond' o a b)
  | evalObs o = evalVal a
  | otherwise = evalVal b

evalObs :: Observation' -> Boolean
evalObs (AndObs' a b) = evalObs a && evalObs b
evalObs (OrObs' a b) = evalObs a || evalObs b
evalObs (NotObs' a) = not $ evalObs a
evalObs (ValueGE' a b) = evalVal a >= evalVal b
evalObs (ValueGT' a b) = evalVal a > evalVal b
evalObs (ValueLT' a b) = evalVal a < evalVal b
evalObs (ValueLE' a b) = evalVal a <= evalVal b
evalObs (ValueEQ' a b) = evalVal a == evalVal b
evalObs TrueObs' = true
evalObs FalseObs' = false

-- | Risk factor observer
data RiskFactors a = RiskFactors
  { o_rf_CURS :: a
  , o_rf_RRMO :: a
  , o_rf_SCMO :: a
  , pp_payoff :: a
  }

-- | Cash flows
data CashFlow a b = CashFlow
  { cashParty :: b
  , cashCounterParty :: b
  , cashPaymentDay :: DateTime
  , cashCalculationDay :: DateTime
  , cashEvent :: EventType
  , amount :: a
  , notional :: a
  , cashCurrency :: String
  }

derive instance Generic (CashFlow a b) _
instance (Show a, Show b) => Show (CashFlow a b) where
  show = genericShow

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

-- == Default instance (Decimal)

setDefaultContractTermValues :: ContractTerms Decimal -> ContractTerms Decimal
setDefaultContractTermValues (ContractTerms ct) = ContractTerms $
  ct
    { scheduleConfig =
        { endOfMonthConvention: applyDefault EOMC_SD ct.scheduleConfig.endOfMonthConvention
        , businessDayConvention: applyDefault BDC_NULL ct.scheduleConfig.businessDayConvention
        , calendar: applyDefault CLDR_NC ct.scheduleConfig.calendar
        }
    , contractPerformance = applyDefault PRF_PF ct.contractPerformance
    , interestCalculationBase = applyDefault IPCB_NT ct.interestCalculationBase
    , premiumDiscountAtIED = applyDefault (fromNumber 0.0) ct.premiumDiscountAtIED
    , scalingEffect = applyDefault SE_OOO ct.scalingEffect
    , penaltyRate = applyDefault (fromNumber 0.0) ct.penaltyRate
    , penaltyType = applyDefault PYTP_O ct.penaltyType
    , prepaymentEffect = applyDefault PPEF_N ct.prepaymentEffect
    , rateSpread = applyDefault (fromNumber 0.0) ct.rateSpread
    , rateMultiplier = applyDefault (fromNumber 1.0) ct.rateMultiplier
    , feeAccrued = applyDefault (fromNumber 0.0) ct.feeAccrued
    , feeRate = applyDefault (fromNumber 0.0) ct.feeRate
    , accruedInterest = applyDefault (fromNumber 0.0) ct.accruedInterest
    , nominalInterestRate = applyDefault (fromNumber 0.0) ct.nominalInterestRate
    , priceAtPurchaseDate = applyDefault (fromNumber 0.0) ct.priceAtPurchaseDate
    , priceAtTerminationDate = applyDefault (fromNumber 0.0) ct.priceAtTerminationDate
    , scalingIndexAtContractDealDate = applyDefault (fromNumber 0.0) ct.scalingIndexAtContractDealDate
    , periodFloor = applyDefault (-infinity) ct.periodFloor
    , periodCap = applyDefault infinity ct.periodCap
    , lifeCap = applyDefault infinity ct.lifeCap
    , lifeFloor = applyDefault (-infinity) ct.lifeFloor
    }
  where
  infinity :: Decimal
  infinity = fromNumber $ 1.0 / 0.0

  applyDefault :: forall a. a -> Maybe a -> Maybe a
  applyDefault v o = o <|> Just v
