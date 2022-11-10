module Actus.Domain.Schedule where

import Data.DateTime (DateTime)
import Data.List (List)

type ShiftedDay =
  { paymentDay :: DateTime
  , calculationDay :: DateTime
  }

--  deriving stock (Eq, Ord, Show, Generic)
--  deriving anyclass (FromJSON, ToJSON)

mkShiftedDay :: DateTime -> ShiftedDay
mkShiftedDay d = { paymentDay: d, calculationDay: d }

type ShiftedSchedule = List ShiftedDay
