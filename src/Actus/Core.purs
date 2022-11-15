-- | Given ACTUS contract terms, cashflows are projected based on risk factors
module Actus.Core
  ( genProjectedCashflows
  ) where

import Prelude

import Actus.Domain (class ActusFrac, class ActusOps, CR(..), CT(..), CashFlow(..), ContractState(..), ContractTerms(..), DS(..), EventType(..), RiskFactors(..), ShiftedDay(..), ShiftedDay)
import Actus.Model.ContractSchedule (maturity, schedule)
import Actus.Model.Payoff (CtxPOF, payoff)
import Actus.Model.StateInitialization (initializeState)
import Actus.Model.StateTransition (CtxSTF, stateTransition)
import Control.Alt ((<|>))
import Control.Monad (map)
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Data.Array (unzip)
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.DateTime (DateTime)
import Data.Enum (enumFromTo, fromEnum, toEnum)
import Data.Enum.Generic (class GenericBoundedEnum, genericFromEnum, genericToEnum)
import Data.Eq ((==))
import Data.Generic.Rep (class Generic)
import Data.List (List(..), concat, concatMap, dropWhile, filter, filterM, foldl, groupBy, mapMaybe, nub, sortBy, zip, (..), (:))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Traversable (mapAccumL, traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))

-- |'genProjectedCashflows' generates a list of projected cashflows for
-- given contract terms and provided risk factors. The function returns
-- an empty list, if building the initial state given the contract terms
-- fails or in case there are no cash flows.
genProjectedCashflows
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Risk factors as a function of event type and time
  (String -> EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms a
  ->
  -- | Unscheduled events
  List Event
  ->
  -- | List of projected cash flows
  List (CashFlow a)
genProjectedCashflows rf ct us =
  let
    ctx = buildCtx rf ct us
  in
    check ct $ genCashflow ct <$> runReader (genProjectedPayoffs us) ctx
  where
  check :: ContractTerms a -> List (CashFlow a) -> List (CashFlow a)
  check (ContractTerms { deliverySettlement: Just DS_S }) = netCashflows
  check _ = \x -> x

  netCashflows :: List (CashFlow a) -> List (CashFlow a)
  netCashflows cf = cf -- FIXME: map (foldl plus) $ groupBy f cf

--    where
--    f (CashFlow a) (CashFlow b) =
--      a.cashEvent == b.cashEvent
--        && a.cashPaymentDay == b.cashPaymentDay
--        && a.cashParty == b.cashParty
--        && a.cashCounterParty == b.cashCounterParty
--        && a.cashCurrency == b.cashCurrency
--    plus a b =
--      a
--        { amount= a.amount + b.amount
--        , notional= a.notional + b.notional
--        }

-- | Bulid the context allowing to perform state transitions
buildCtx
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Risk factors as a function of event type and time
  (String -> EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms a
  ->
  -- | Unscheduled events
  List Event
  ->
  -- | Context
  CtxSTF a
buildCtx rf ct us =
  { contractTerms: ct
  , fpSchedule: (_.calculationDay <$> schedule FP ct)
  , -- init & stf rely on the fee payment schedule
    prSchedule: (_.calculationDay <$> schedule PR ct)
  , -- init & stf rely on the principal redemption schedule
    ipSchedule: (_.calculationDay <$> schedule IP ct)
  , -- init & stf rely on the interest payment schedule
    maturity: (maturity ct)
  , riskFactors: rf
  }

-- |Generate cash flows
genCashflow
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Contract terms
  ContractTerms a
  ->
  -- | Projected payoff
  Event /\ ContractState a /\ a
  ->
  -- | Projected cash flow
  CashFlow a
genCashflow (ContractTerms { currency }) ((ev /\ { paymentDay, calculationDay }) /\ ContractState { nt } /\ am) =
  CashFlow
    { tick: 0
    , cashParty: "party"
    , cashCounterParty: "counterparty"
    , cashPaymentDay: paymentDay
    , cashCalculationDay: calculationDay
    , cashEvent: ev
    , amount: am
    , notional: nt
    , cashCurrency: fromMaybe "unknown" currency
    }

-- |Generate projected cash flows
genProjectedPayoffs
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Unscheduled events
  List Event
  -> Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs us =
  do
    ct <- _.contractTerms <$> ask
    genProjectedPayoffs' $ genSchedule ct us

-- |Generate projected cash flows
genProjectedPayoffs'
  :: forall a
   . ActusOps a
  =>
  -- | Events
  List Event
  ->
  -- | Projected cash flows
  Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs' events = pure Nil -- FIXME

-- |Generate schedules
genSchedule
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Contract terms
  ContractTerms a
  ->
  -- | Schedule
  List Event
  ->
  -- | Schedule
  List Event
genSchedule ct us =
  sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ genFixedSchedule ct <> us

genFixedSchedule
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Contract terms
  ContractTerms a
  ->
  -- | Schedule
  List Event
genFixedSchedule ct@(ContractTerms { terminationDate, statusDate }) =
  -- filter filtersSchedules <<< postProcessSchedules <<< sortBy (\(_ /\ ev /\ {paymentDay}) -> (paymentDay /\ ev)) $
  sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ concatMap scheduleEvent allElements
  where

  allElements :: forall a rep. Generic a rep => GenericBoundedEnum rep => GenericTop rep => GenericBottom rep => List a
  allElements = mapMaybe genericToEnum (idxFrom .. idxTo)
    where
    idxFrom = genericFromEnum (genericBottom :: a)
    idxTo = genericFromEnum (genericTop :: a)

  scheduleEvent ev = map (\d -> (ev /\ d)) $ schedule ev ct

--  filtersSchedules :: Event -> Boolean
--  filtersSchedules (_ /\ _ /\ {calculationDay}) = isNothing terminationDate || Just calculationDay <= terminationDate

-- FIXME:
--  postProcessSchedules :: List Event -> List Event
--  postProcessSchedules =
--    let
--      trim = dropWhile (\(_ /\ _ /\ {calculationDay}) -> calculationDay < statusDate)
--      regroup = groupBy (\(_ /\ _ /\ {calculationDay:l}) (_ /\ _ /\ {calculationDay:r}) -> l == r)
--      overwrite = map (sortBy (\(_ /\ ev /\ _) -> fromEnum ev)).regroup
--    in
--      concat <<< overwrite <<< trim

type Event = EventType /\ ShiftedDay

-- |Generate states
genStates
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Schedules
  List Event
  ->
  -- | Initial state
  ContractState a
  ->
  -- | New states
  Reader (CtxSTF a) (List (Event /\ ContractState a))
genStates scs stn@(ContractState { sd: statusDate }) = pure Nil -- FIXME
  where
  st0 :: Event /\ ContractState a
  st0 = (AD /\ { calculationDay: statusDate, paymentDay: statusDate }) /\ stn

filtersStates
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  => ((EventType /\ ShiftedDay) /\ ContractState a)
  -> Reader (CtxSTF a) Boolean
filtersStates ((ev /\ { calculationDay }) /\ _) =
  do
    ct'@(ContractTerms ct) <- _.contractTerms <$> ask
    pure $ case ct.contractType of
      PAM -> isNothing ct.purchaseDate || Just calculationDay >= ct.purchaseDate
      LAM -> isNothing ct.purchaseDate || ev == PRD || Just calculationDay > ct.purchaseDate
      NAM -> isNothing ct.purchaseDate || ev == PRD || Just calculationDay > ct.purchaseDate
      ANN ->
        let
          b1 = isNothing ct.purchaseDate || ev == PRD || Just calculationDay > ct.purchaseDate
          b2 = let m = ct.maturityDate <|> ct.amortizationDate <|> maturity ct' in isNothing m || Just calculationDay <= m
        in
          b1 && b2

-- |Generate payoffs
genPayoffs
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | States with schedule
  List Event
  ->
  -- | States with schedule
  List (ContractState a)
  ->
  -- | Payoffs
  Reader (CtxPOF a) (List a)
genPayoffs evs sts = traverse calculatePayoff $ zip evs sts
  where
  calculatePayoff ((ev /\ { calculationDay }) /\ st) = payoff (ev /\ calculationDay) st
