module Actus.Domain.BusinessEvents where

import Prelude

import Contrib.Data.Argonaut (decodeJsonEnumWith, encodeJsonEnumWith)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum, class BoundedEnum)
import Data.Enum.Generic (genericCardinality, genericPred, genericSucc, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | ACTUS event types, https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-event.json
data EventType
  = IED -- ^ Initial Exchange
  | FP -- ^ Fee Payment
  | PR -- ^ Principal Redemption
  | PD -- ^ Principal Drawing
  | PY -- ^ Penalty Payment
  | PP -- ^ Principal Prepayment (unscheduled event)
  | IP -- ^ Interest Payment
  | IPFX -- ^ Interest Payment Fixed Leg
  | IPFL -- ^ Interest Payment Floating Leg
  | IPCI -- ^ Interest Capitalization
  | CE -- ^ Credit Event
  | RRF -- ^ Rate Reset Fixing with Known Rate
  | RR -- ^ Rate Reset Fixing with Unknown Rate
  | PRF -- ^ Principal Payment Amount Fixing
  | DV -- ^ Dividend Payment
  | PRD -- ^ Purchase
  | MR -- ^ Margin Call
  | TD -- ^ Termination
  | SC -- ^ Scaling Index Fixing
  | IPCB -- ^ Interest Calculation Base Fixing
  | MD -- ^ Maturity
  | XD -- ^ Exercise
  | STD -- ^ Settlement
  | PI -- ^ Principal Increase
  | AD -- ^ Monitoring

derive instance Generic EventType _
derive instance Eq EventType
derive instance Ord EventType

instance Show EventType where
  show = genericShow

instance Enum EventType where
  succ = genericSucc
  pred = genericPred

instance Bounded EventType where
  top = genericTop
  bottom = genericBottom

instance BoundedEnum EventType where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance EncodeJson EventType where
  encodeJson = encodeJsonEnumWith identity

instance DecodeJson EventType where
  decodeJson = decodeJsonEnumWith identity
