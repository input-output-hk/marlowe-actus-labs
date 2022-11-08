module Actus.Domain.Schedule where

import Data.DateTime (DateTime)
import Data.List (List)

newtype ShiftedDay = ShiftedDay
  { paymentDay     :: DateTime,
    calculationDay :: DateTime
  }

--  deriving stock (Eq, Ord, Show, Generic)
--  deriving anyclass (FromJSON, ToJSON)

mkShiftedDay :: DateTime -> ShiftedDay
mkShiftedDay d = ShiftedDay {paymentDay: d, calculationDay: d}

type ShiftedSchedule = List ShiftedDay
