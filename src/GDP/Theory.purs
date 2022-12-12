module GDP.Theory
  ( (...)
  , Defn
  , Named
  , Proof
  , Satisfies
  , SuchThat
  , assert
  , attach
  , axiom
  , class Defining
  , class The
  , defn
  , detach
  , name
  , rename
  , the
  , type (:::)
  , type (?)
  , type (~~)
  , unname
  ) where

import Prelude

import Prim.Coerce (class Coercible)

newtype Named :: forall k. Type -> k -> Type
newtype Named a name = Named a

type role Named nominal nominal

infix 5 type Named as ~~

data Proof :: forall k. k -> Type
data Proof p = QED

type role Proof nominal

newtype SuchThat :: forall k. Type -> k -> Type
newtype SuchThat a p = SuchThat a

type role SuchThat nominal nominal

newtype Satisfies :: forall k. Type -> (k -> k) -> Type
newtype Satisfies a p = Satisfies a

infix 4 type Satisfies as ?

infix 4 type SuchThat as :::

data Defn = Defn

class Defining :: forall k. k -> Constraint
class Defining a

instance (Coercible a Defn, Coercible Defn a) => Defining a

class The d a | d -> a where
  the :: d -> a

instance The (a ~~ n) a where
  the (Named x) = x

instance The (a ::: p) a where
  the (SuchThat x) = x

instance The (a ? p) a where
  the (Satisfies x) = x

name :: forall a r. a -> (forall name. a ~~ name -> r) -> r
name x k = k (Named x)

unname :: forall a n p. a ~~ n ::: p n -> a ? p
unname = Satisfies <<< the <<< the

rename :: forall a p r. a ? p -> (forall n. a ~~ n ::: p n -> r) -> r
rename x k = k $ SuchThat $ Named $ the x

defn :: forall a n. Defining n => a -> a ~~ n
defn = Named

axiom :: forall p. Defining p => Proof p
axiom = QED

attach :: forall a p. a -> Proof p -> SuchThat a p
attach x _ = SuchThat x

infix 4 attach as ...

detach :: forall a p. SuchThat a p -> Proof p
detach _ = QED

assert :: forall a p. Defining (p Unit) => a -> a ? p
assert = Satisfies
