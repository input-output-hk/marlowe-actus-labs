module Actus.Model.Payoff
  ( CtxPOF(..)
  , payoff
  ) where

import Actus.Domain (class ActusOps, CT(..), ContractState(..), ContractTerms(..), EventType(..), FEB(..), PYTP(..), RiskFactors(..), _abs, _max, sign)
import Actus.Utility.YearFraction (yearFraction)
import Control.Monad.Reader (Reader, asks)
import Data.Array (elem)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Prelude

-- |The context for payoff functions
type CtxPOF a =
  { -- | Contract terms
    contractTerms :: ContractTerms a
  ,
    -- | Risk factors as a function of event type and time
    riskFactors :: String -> EventType -> DateTime -> RiskFactors a
  }

-- | The payoff function
payoff
  :: forall a
   . Semiring a
  => EuclideanRing a
  => ActusOps a
  =>
  -- | Event
  EventType /\ DateTime
  ->
  -- | Contract state
  ContractState a
  ->
  -- | Updated contract state
  Reader (CtxPOF a) a
payoff (ev /\ t) st = asks $ do \ctx -> let terms@(ContractTerms ct :: ContractTerms a) = ctx.contractTerms in pof ev (ctx.riskFactors ct.contractId ev t) terms st
  where
  ----------------------------
  -- Initial Exchange (IED) --
  ----------------------------
  pof :: forall a. Semiring a => EuclideanRing a => ActusOps a => EventType -> RiskFactors a -> ContractTerms a -> ContractState a -> a
  -- POF_IED_*
  pof
    IED
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { notionalPrincipal: Just nt
        , premiumDiscountAtIED: Just pdied
        , contractRole
        }
    )
    _ = negate $ o_rf_CURS * sign contractRole * (nt + pdied)
  -- POF_IED_*
  pof
    IED
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { notionalPrincipal: Just nt
        , contractRole
        }
    )
    _ = negate $ o_rf_CURS * sign contractRole * nt
  -------------------------------
  -- Principal Redemption (PR) --
  -------------------------------
  -- POF_PR_LAM
  pof
    PR
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType: LAM
        , contractRole
        }
    )
    ( ContractState
        { nt
        , nsc
        , prnxt
        }
    ) =
    let
      redemption = prnxt - sign contractRole * _max zero (_abs prnxt - _abs nt)
    in
      o_rf_CURS * sign contractRole * nsc * redemption
  -- POF_PR_NAM
  -- POF_PR_ANN
  pof
    PR
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType
        , dayCountConvention: Just dcc
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { nt
        , nsc
        , prnxt
        , ipac
        , ipcb
        , ipnr
        , sd
        }
    )
    | contractType `elem` [ NAM, ANN ] =
        let
          timeFromLastEvent = yearFraction dcc sd t maturityDate
          ra = prnxt - sign contractRole * (ipac + timeFromLastEvent * ipnr * ipcb)
          r = ra - _max zero (ra - _abs nt)
        in
          o_rf_CURS * sign contractRole * nsc * r
  -------------------
  -- Maturity (MD) --
  -------------------
  -- POF_IED_*
  pof
    MD
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType
        }
    )
    ( ContractState
        { nt
        , nsc
        , isc
        , ipac
        , feac
        }
    ) = o_rf_CURS * (nsc * nt + isc * ipac + feac)
  -------------------------------
  -- Principal Prepayment (PP) --
  -------------------------------
  -- POF_PP_*
  pof
    PP
    ( RiskFactors
        { o_rf_CURS
        , pp_payoff
        }
    )
    _
    _ = o_rf_CURS * pp_payoff
  --------------------------
  -- Penalty Payment (PY) --
  --------------------------
  -- POF_PY_*
  pof
    PY
    ( RiskFactors
        { o_rf_CURS
        , o_rf_RRMO
        }
    )
    ( ContractTerms
        { penaltyType: Just pytp
        , penaltyRate: Just pyrt
        , dayCountConvention: Just dcc
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { nt
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      case pytp of
        PYTP_A -> o_rf_CURS * sign contractRole * pyrt
        PYTP_N -> let c = o_rf_CURS * sign contractRole * timeFromLastEvent * nt in c * pyrt
        PYTP_I -> let c = o_rf_CURS * sign contractRole * timeFromLastEvent * nt in c * _max zero (ipnr - o_rf_RRMO)
        PYTP_O -> o_rf_CURS * sign contractRole * pyrt
  -- Fee Payment (FP) --
  ----------------------
  -- POF_FP_*
  pof
    FP
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { dayCountConvention: Just dcc
        , feeBasis: Just feb
        , feeRate: Just fer
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { nt
        , feac
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      case feb of
        FEB_A -> sign contractRole * o_rf_CURS * fer
        FEB_N -> o_rf_CURS * fer * timeFromLastEvent * nt * feac
  --------------------
  -- Purchase (PRD) --
  --------------------
  -- POF_PRD_PAM
  pof
    PRD
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType: PAM
        , dayCountConvention: Just dcc
        , priceAtPurchaseDate: Just pprd
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { nt
        , ipac
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      negate $ o_rf_CURS * sign contractRole * (pprd + ipac + timeFromLastEvent * ipnr * nt)
  -- POF_PRD_LAM
  -- POF_PRD_NAM
  -- POF_PRD_ANN
  pof
    PRD
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { dayCountConvention: Just dcc
        , priceAtPurchaseDate: Just pprd
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { ipac
        , ipcb
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      negate $ o_rf_CURS * sign contractRole * (pprd + ipac + timeFromLastEvent * ipnr * ipcb)
  ----------------------
  -- Termination (TD) --
  ----------------------
  -- POF_TD_PAM
  pof
    TD
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType: PAM
        , dayCountConvention: Just dcc
        , priceAtTerminationDate: Just ptd
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { nt
        , ipac
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      o_rf_CURS * sign contractRole * (ptd + ipac + timeFromLastEvent * ipnr * nt)
  -- POF_TD_*
  pof
    TD
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { dayCountConvention: Just dcc
        , priceAtTerminationDate: Just ptd
        , maturityDate
        , contractRole
        }
    )
    ( ContractState
        { ipac
        , ipcb
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      o_rf_CURS * sign contractRole * (ptd + ipac + timeFromLastEvent * ipnr * ipcb)
  ---------------------------
  -- Interest Payment (IP) --
  ---------------------------
  -- POF_IP_PAM
  pof
    IP
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { contractType: PAM
        , dayCountConvention: Just dcc
        , maturityDate
        }
    )
    ( ContractState
        { nt
        , isc
        , ipac
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      o_rf_CURS * isc * (ipac + timeFromLastEvent * ipnr * nt)
  -- POF_IP_*
  pof
    IP
    ( RiskFactors
        { o_rf_CURS
        }
    )
    ( ContractTerms
        { dayCountConvention: Just dcc
        , maturityDate
        }
    )
    ( ContractState
        { isc
        , ipac
        , ipcb
        , ipnr
        , sd
        }
    ) =
    let
      timeFromLastEvent = yearFraction dcc sd t maturityDate
    in
      o_rf_CURS * isc * (ipac + timeFromLastEvent * ipnr * ipcb)
  -------------
  -- Default --
  -------------
  pof _ _ _ _ = zero
