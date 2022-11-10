module Actus.Utility.DateShift
  ( applyBDC
  , applyBDCWithCfg
  , applyEOMC
  , getFollowingBusinessDay
  , getPreceedingBusinessDay
  , moveToEndOfMonth
  , shiftDate
  ) where

import Prelude

import Actus.Domain (BDC(..), Calendar(..), Cycle(..), EOMC(..), Period(..), ScheduleConfig(..), ShiftedDay(..))
import Data.Array ((!!))
import Data.Date (Date(..), Day, Month, Weekday(..), Year, adjust, canonicalDate, day, isLeapYear, lastDayOfMonth, month, weekday, year)
import Data.DateTime (DateTime(..), date)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Refined (fromInt)
import Data.Time.Duration (Days(..))
import Data.Tuple.Nested (Tuple3(..), Tuple3)

{- Business Day Convention -}

applyBDCWithCfg :: ScheduleConfig -> DateTime -> ShiftedDay
applyBDCWithCfg
  { businessDayConvention: Just bdc
  , calendar: Just cal
  }
  d = applyBDC bdc cal d
applyBDCWithCfg _ date = { paymentDay: date, calculationDay: date }

applyBDC :: BDC -> Calendar -> DateTime -> ShiftedDay
applyBDC BDC_NULL _ date = { paymentDay: date, calculationDay: date }

applyBDC BDC_SCF cal date =
  { paymentDay: getFollowingBusinessDay date cal
  , calculationDay: getFollowingBusinessDay date cal
  }

applyBDC BDC_SCMF cal date =
  { paymentDay: shiftModifiedFollowing date cal
  , calculationDay: shiftModifiedFollowing date cal
  }

applyBDC BDC_CSF cal date =
  { paymentDay: getFollowingBusinessDay date cal
  , calculationDay: date
  }

applyBDC BDC_CSMF cal date =
  { paymentDay: shiftModifiedFollowing date cal
  , calculationDay: date
  }

applyBDC BDC_SCP cal date =
  { paymentDay: getPreceedingBusinessDay date cal
  , calculationDay: getPreceedingBusinessDay date cal
  }

applyBDC BDC_SCMP cal date =
  { paymentDay: shiftModifiedPreceeding date cal
  , calculationDay: shiftModifiedPreceeding date cal
  }

applyBDC BDC_CSP cal date =
  { paymentDay: getPreceedingBusinessDay date cal
  , calculationDay: date
  }

applyBDC BDC_CSMP cal date =
  { paymentDay: shiftModifiedPreceeding date cal
  , calculationDay: date
  }

shiftModifiedFollowing :: DateTime -> Calendar -> DateTime
shiftModifiedFollowing dt@(DateTime d t) cal =
  let
    m = month d
    st@(DateTime d' t') = getFollowingBusinessDay dt cal
    shiftedMonth = month d'
  in
    if m == shiftedMonth then st else getPreceedingBusinessDay dt cal

shiftModifiedPreceeding :: DateTime -> Calendar -> DateTime
shiftModifiedPreceeding dt@(DateTime d t) cal =
  let
    m = month d
    st@(DateTime d' t') = getPreceedingBusinessDay dt cal
    shiftedMonth = month d'
  in
    if m == shiftedMonth then st else getFollowingBusinessDay dt cal

getFollowingBusinessDay :: DateTime -> Calendar -> DateTime
getFollowingBusinessDay (DateTime d t) CLDR_MF =
  let
    d' = case weekday d of
      Saturday -> addDays 2 d
      Sunday -> addDays 1 d
      _ -> d
  in
    DateTime d' t
getFollowingBusinessDay dt _ = dt

getPreceedingBusinessDay :: DateTime -> Calendar -> DateTime
getPreceedingBusinessDay (DateTime d t) CLDR_MF =
  let
    d' = case weekday d of
      Saturday -> addDays (-1) d
      Sunday -> addDays (-2) d
      _ -> d
  in
    DateTime d' t
getPreceedingBusinessDay dt _ = dt

shiftDate :: DateTime -> Int -> Period -> DateTime
shiftDate (DateTime d t) n p =
  DateTime
    ( case p of
        P_D -> addDays n d
        P_W -> addDays (n * 7) d
        P_M -> addGregorianMonthsClip n d
        P_Q -> addGregorianMonthsClip (n * 3) d
        P_H -> addGregorianMonthsClip (n * 6) d
        P_Y -> addGregorianYearsClip n d
    )
    t

{- End of Month Convention -}
applyEOMC :: DateTime -> Cycle -> EOMC -> DateTime -> DateTime
applyEOMC s (Cycle cycle) endOfMonthConvention dt
  | isLastDayOfMonthWithLessThan31Days s
      && cycle.p /= P_D
      && cycle.p /= P_W
      && endOfMonthConvention == EOMC_EOM = moveToEndOfMonth dt
  | otherwise = dt

isLastDayOfMonthWithLessThan31Days :: DateTime -> Boolean
isLastDayOfMonthWithLessThan31Days (DateTime d _) = Just (day d) < toEnum 31 && isLastDayOfMonth d

moveToEndOfMonth :: DateTime -> DateTime
moveToEndOfMonth (DateTime d t) = DateTime (canonicalDate (year d) (month d) (lastDayOfMonth (year d) (month d))) t

isLastDayOfMonth :: Date -> Boolean
isLastDayOfMonth d = lastDayOfMonth (year d) (month d) == (day d)

addDays :: Int -> Date -> Date
addDays n d = fromMaybe d $ adjust (Days $ fromInt n) d

addGregorianMonthsClip :: Int -> Date -> Date
addGregorianMonthsClip n d = d -- FIXME

addGregorianYearsClip :: Int -> Date -> Date
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)
