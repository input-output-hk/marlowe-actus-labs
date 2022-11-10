module Actus.Utility.YearFraction
  ( yearFraction
  ) where

import Prelude

import Actus.Domain.ContractTerms (DCC(..))
import Data.Date (Date, diff)
import Data.DateTime (DateTime, date)
import Data.Int (ceil)
import Data.Maybe (Maybe)
import Data.Refined (fromInt)
import Data.Time.Duration (Days(..))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

yearFraction :: forall a. EuclideanRing a => DCC -> DateTime -> DateTime -> Maybe DateTime -> a
yearFraction dcc x y o = yearFraction' dcc (date x) (date $ clipToMidnight y) (date <$> o)

yearFraction' :: forall a. EuclideanRing a => DCC -> Date -> Date -> Maybe Date -> a
-- yearFraction' DCC_A_AISDA startDay endDay _
--   | startDay <= endDay
--   = let
--       d1Year = year startDay
--       d2Year = year endDay
--       d1YearFraction = (if isLeapYear d1Year then fromInt 366 else fromInt 365)
--     in
--       if d1Year == d2Year
--         then (diff endDay startDay) / d1YearFraction
--         else
--           let
--             d2YearFraction = (if isLeapYear d2Year then fromInt 366 else fromInt 365)
--             d1YearLastDay      = canonicalDate (d1Year + 1) January (toEnum 1)
--             d2YearLastDay      = canonicalDate d2Year January (toEnum 1)
--             firstFractionDays  = diff d1YearLastDay startDay
--             secondFractionDays = diff endDay d2YearLastDay
--           in
--             (firstFractionDays / d1YearFraction)
--               + (secondFractionDays / d2YearFraction) + d2Year - d1Year - 1
--   | otherwise
--   = 0.0

yearFraction' DCC_A_360 startDay endDay _
  | startDay <= endDay = let Days daysDiff = diff endDay startDay in (fromInt $ ceil daysDiff) / fromInt 360
  | otherwise = zero

yearFraction' DCC_A_365 startDay endDay _
  | startDay <= endDay = let Days daysDiff = diff endDay startDay in (fromInt $ ceil daysDiff) / fromInt 365
  | otherwise = zero

-- yearFraction' DCC_E30_360ISDA _ _ Nothing = error "DCC_E30_360ISDA requires maturity date"
-- yearFraction' DCC_E30_360ISDA startDay endDay (Just maturityDate)
--   | startDay <= endDay
--   = let
--       d1ChangedDay = if isLastDayOfMonth startDay then 30 else day startDay
--       d2ChangedDay = if isLastDayOfMonth endDay && not (endDay == maturityDate && (month endDay == 2)) then 30 else day endDay
--     in
--       ( 360.0
--         * ((year endDay) - (year startDay))
--         + 30.0
--         * ((month endDay) - (month startDay))
--         + (d2ChangedDay - d1ChangedDay)
--         )
--         / 360.0
--   | otherwise
--   = 0.0

-- yearFraction' DCC_E30_360 startDay endDay _
--   | startDay <= endDay
--   = let d1ChangedDay             = if day startDay == 31 then 30 else day startDay
--         d2ChangedDay             = if day endDay == 31 then 30 else day endDay
--     in  ( 360.0
--         * ((year endDay) - (year startDay))
--         + 30.0
--         * ((month endDay) - (month startDay))
--         + (d2ChangedDay - d1ChangedDay)
--         )
--           / 360.0
--   | otherwise
--   = 0.0

yearFraction' dcc _ _ _ = error $ "Unsupported day count convention: " <> show dcc

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

-- isLastDayOfMonth :: Date -> Boolean
-- isLastDayOfMonth d = lastDayOfMonth (year d) (month d) == (day d)

-- |Advance to midnight, if one second before midnight - see note in ACTUS specification (2.8. Date/Time)
clipToMidnight :: DateTime -> DateTime
-- clipToMidnight (DateTime _ t) | Just (hour t) == toEnum 23 && Just (minute t) == toEnum 59 && Just (second t) == toEnum 59 = adjust ...
clipToMidnight lt = lt
