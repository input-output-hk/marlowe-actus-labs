-- | Given ACTUS contract terms, cashflows are projected based on risk factors
module Actus.Core
  ( genProjectedCashflows
  ) where

import Prelude

import Actus.Domain (class ActusFrac, class ActusOps, CT(..), CashFlow(..), ContractState(..), ContractTerms(..), DS(..), EventType(..), RiskFactors, ShiftedDay)
import Actus.Model.ContractSchedule (maturity, schedule)
import Actus.Model.Payoff (CtxPOF, payoff)
import Actus.Model.StateInitialization (initializeState)
import Actus.Model.StateTransition (CtxSTF, stateTransition)
import Control.Alt ((<|>))
import Control.Monad.Reader (Reader, ask, runReader, withReader)
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.DateTime (DateTime)
import Data.Enum (fromEnum)
import Data.Enum.Generic (class GenericBoundedEnum, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), concat, concatMap, dropWhile, filter, filterM, groupBy, mapMaybe, unzip, zip, (..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, toList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Semigroup.Foldable (foldl1)
import Data.Traversable (traverse)
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
  (EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms a
  ->
  -- | List of projected cash flows
  List (CashFlow a)
genProjectedCashflows rf ct =
  let
    ctx = buildCtx rf ct
  in
    check ct $ genCashflow ct <$> runReader genProjectedPayoffs ctx
  where
  check :: ContractTerms a -> List (CashFlow a) -> List (CashFlow a)
  check (ContractTerms { deliverySettlement: Just DS_S }) = netCashflows
  check _ = \x -> x

  groupCashflows :: List (CashFlow a) -> List (NonEmptyList (CashFlow a))
  groupCashflows cf = groupBy f cf
    where
    f (CashFlow a) (CashFlow b) =
      a.cashEvent == b.cashEvent
        && a.cashPaymentDay == b.cashPaymentDay
        && a.cashParty == b.cashParty
        && a.cashCounterParty == b.cashCounterParty
        && a.cashCurrency == b.cashCurrency

  netCashflows cf = map (foldl1 plus) $ groupCashflows cf
    where
    plus :: CashFlow a -> CashFlow a -> CashFlow a
    plus (CashFlow a) (CashFlow b) = CashFlow $
      a
        { amount = a.amount + b.amount
        , notional = a.notional + b.notional
        }

-- | Bulid the context allowing to perform state transitions
buildCtx
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Risk factors as a function of event type and time
  (EventType -> DateTime -> RiskFactors a)
  ->
  -- | Contract terms
  ContractTerms a
  ->
  -- | Context
  CtxSTF a
buildCtx rf ct =
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
  => Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs =
  do
    ct <- _.contractTerms <$> ask
    genProjectedPayoffs' $ genSchedule ct

-- |Generate projected cash flows
genProjectedPayoffs'
  :: forall a
   . ActusOps a
  => EuclideanRing a
  => ActusFrac a
  =>
  -- | Events
  List Event
  ->
  -- | Projected cash flows
  Reader (CtxSTF a) (List (Event /\ ContractState a /\ a))
genProjectedPayoffs' events =
  do
    st0 <- initializeState
    states <- genStates events st0

    let (x /\ y) = unzip states
    payoffs <- trans $ genPayoffs x y

    pure $ zip x (zip y payoffs)
  where
  trans :: forall b. Reader (CtxPOF a) b -> Reader (CtxSTF a) b
  trans = withReader (\{ contractTerms, riskFactors } -> { contractTerms, riskFactors })

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
genSchedule ct =
  List.sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ genFixedSchedule ct

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
  filter filtersSchedules <<< postProcessSchedules <<< List.sortBy (comparing \(ev /\ { paymentDay }) -> (paymentDay /\ ev)) $ concatMap scheduleEvent allElements
  where

  allElements :: forall b rep. Generic b rep => GenericBoundedEnum rep => GenericTop rep => GenericBottom rep => List b
  allElements = mapMaybe genericToEnum (idxFrom .. idxTo)
    where
    idxFrom = genericFromEnum (genericBottom :: b)
    idxTo = genericFromEnum (genericTop :: b)

  scheduleEvent ev = map (\d -> (ev /\ d)) $ schedule ev ct

  filtersSchedules :: Event -> Boolean
  filtersSchedules (_ /\ { calculationDay }) = isNothing terminationDate || Just calculationDay <= terminationDate

  postProcessSchedules :: List Event -> List Event
  postProcessSchedules =
    let
      trim :: List Event -> List Event
      trim = dropWhile (\(_ /\ { calculationDay }) -> calculationDay < statusDate)

      regroup :: List Event -> List (NonEmptyList Event)
      regroup = groupBy (\(_ /\ { calculationDay: l }) (_ /\ { calculationDay: r }) -> l == r)

      overwrite :: List (NonEmptyList Event) -> List (NonEmptyList Event)
      overwrite = map f

      f :: NonEmptyList Event -> NonEmptyList Event
      f = NonEmptyList.sortBy g

      g :: Event -> Event -> Ordering
      g = comparing $ \(ev /\ _) -> fromEnum ev
    in
      concat <<< map toList <<< overwrite <<< regroup <<< trim

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
genStates scs stn@(ContractState { sd: statusDate }) = mapAccumLM' apply st0 scs >>= filterM filtersStates <<< snd
  where
  apply ((ev /\ { calculationDay }) /\ st) (ev' /\ t') =
    do
      newState <- stateTransition ev calculationDay st
      pure (((ev' /\ t') /\ newState) /\ ((ev' /\ t') /\ newState))

  st0 :: Event /\ ContractState a
  st0 = (AD /\ { calculationDay: statusDate, paymentDay: statusDate }) /\ stn

mapAccumLM' :: forall acc x y m. Monad m => (acc -> x -> m (acc /\ y)) -> acc -> List x -> m (acc /\ List y)
mapAccumLM' f = go
  where
  go s (x : xs) = do
    (s1 /\ x') <- f s x
    (s2 /\ xs') <- go s1 xs
    pure (s2 /\ x' : xs')
  go s Nil = pure (s /\ Nil)

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
