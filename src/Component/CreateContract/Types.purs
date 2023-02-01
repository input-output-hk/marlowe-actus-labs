module Component.CreateContract.Types where

import Prelude

import Actus.Domain (ContractTerms(..))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Actus (CashFlows)
import Type.Row (type (+))

data ContractFormTypeChoice
  = AmortizingLoans
  | JsonForm
  | Bonds
  | BulletLoans
  | CapitalizingLoans
  | ZeroCouponBonds

derive instance Eq ContractFormTypeChoice
derive instance Ord ContractFormTypeChoice
derive instance Generic ContractFormTypeChoice _
instance Show ContractFormTypeChoice where
  show = genericShow
instance Enum ContractFormTypeChoice where
  succ = genericSucc
  pred = genericPred
instance Bounded ContractFormTypeChoice where
  bottom = genericBottom
  top = genericTop
instance BoundedEnum ContractFormTypeChoice where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

data AmortizingLoanChoice
  = PrincipalAtMaturity
  | LinearAmortizer
  | NegativeAmortizer
  | Annuity

derive instance Eq AmortizingLoanChoice
derive instance Ord AmortizingLoanChoice
derive instance Generic AmortizingLoanChoice _
instance Show AmortizingLoanChoice where
  show = genericShow
instance Enum AmortizingLoanChoice where
  succ = genericSucc
  pred = genericPred
instance Bounded AmortizingLoanChoice where
  bottom = genericBottom
  top = genericTop
instance BoundedEnum AmortizingLoanChoice where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

type ThirdStepBaseRow r =
  ( contractTerms :: ContractTerms
  , contractFormType :: ContractFormTypeChoice
  | r
  )

type FourthStepBaseRow r =
  ( cashFlows :: CashFlows
  , contract :: V1.Contract
  , counterParty :: V1.Party
  , party :: V1.Party
  | ThirdStepBaseRow + r
  )

data WizzardStep
  = FirstStep
  | SecondStep ContractFormTypeChoice
  | ThirdStep { | ThirdStepBaseRow () }
  | FourthStep { | FourthStepBaseRow () }

