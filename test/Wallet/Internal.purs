module Test.Wallet.Internal
  ( spec
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, ForeignError)
import Foreign as Foreign
import Foreign.Object (Object)
import Foreign.Object as Object
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions (fail)
import Wallet.Internal as Wallet.Internal

foreign import eqeqeq :: forall a. a -> a -> Boolean

infixr 5 eqeqeq as ===

success :: forall m. Monad m => m Unit
success = pure unit

spec :: Spec Unit
spec = do
  Spec.describeOnly "Wallet" do
    Spec.describe "readCardano" do
      Spec.it "returns nothing when field 'cardano' is unset" do
        actual :: Either (NonEmptyList ForeignError) (Maybe (Object Foreign)) <-
          runExceptT $ Wallet.Internal.readCardano $ Foreign.unsafeToForeign {}
        case actual of
          Right Nothing -> success
          Right (Just _) -> fail "actual Just _ /= expected Nothing"
          Left errors -> fail $ show errors

      Spec.it "returns just when field 'cardano' is set with object" do
        actual :: Either (NonEmptyList ForeignError) (Maybe (Object Foreign)) <-
          runExceptT $ Wallet.Internal.readCardano $ Foreign.unsafeToForeign { cardano: {} }
        case actual of
          Right Nothing -> fail "actual Nothing /= expected Just _"
          Right (Just _) -> success
          Left errors -> fail $ show errors

      Spec.it "returns same object as field 'cardano' when it is set with object" do
        let obj1 :: Object Foreign
            obj1 = Object.empty
        actual :: Either (NonEmptyList ForeignError) (Maybe (Object Foreign)) <-
          runExceptT $ Wallet.Internal.readCardano $ Foreign.unsafeToForeign { cardano: obj1 }
        case actual of
          Right Nothing -> fail "actual Nothing /= expected Just _"
          Right (Just obj2)
            | obj1 === obj2 -> success
            | otherwise -> fail "expected pointer equality"
          Left errors -> fail $ show errors
