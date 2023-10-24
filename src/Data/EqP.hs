{-# LANGUAGE CPP                   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module Data.EqP (
    EqP (..),
) where

import Control.Applicative   (Const (..))
import Data.Kind             (Type)
import Data.Proxy            (Proxy (..))
import Data.Type.Equality    ((:~:) (..), (:~~:) (..))
import GHC.Generics          ((:*:) (..), (:+:) (..))
import System.Mem.StableName (StableName, eqStableName)

#if MIN_VERSION_base(4,18,0)
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import qualified GHC.TypeLits as TL
import qualified GHC.TypeNats as TN
#endif

#if !MIN_VERSION_base(4,19,0)
import Data.Orphans ()
#endif

import qualified Type.Reflection as TR

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint)
#endif

-- | Heterogenous lifted equality.
--
-- This class is stronger version of 'Eq1' from @base@
--
-- @
-- class (forall a. Eq a => Eq (f a)) => Eq1 f where
--     liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool
-- @
--
-- as we don't require a @a -> b -> Bool@ function.
--
-- Morally 'Eq1' should be a superclass of 'EqP', but it cannot be,
-- as GHC wouldn't allow 'EqP' to be polykinded.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/22682
--
-- == Laws
--
-- [reflexivity]    @'eqp' x x ≡ True@
-- [symmetry]       @'eqp' x y ≡ 'eqp' y x@
-- [transitivity]   @'eqp' x y ≡ 'eqp' y z ≡ True ⇒ 'eqp' x z ≡ True@
-- [compatibility]  @'eqp' x y ≡ x '==' y@
-- [extensionality] @'eqp' x y ≡ True ⇒ f x == f y ≡ True@ for polymorphic @f :: forall x. f x -> a@ and @'Eq' a@.
--
-- /Note:/ P stands for phantom.
--
-- @since 1.0.5
#if __GLASGOW_HASKELL__ >= 810
type EqP :: (k -> Type) -> Constraint
#endif
class (forall a. Eq (f a)) => EqP (f :: k -> Type) where
    eqp :: f a -> f b -> Bool

instance EqP ((:~:) a) where
    eqp _ _ = True

instance EqP ((:~~:) a) where
    eqp _ _ = True


#if MIN_VERSION_base(4,18,0)
instance (EqP a, EqP b) => EqP (Sum a b) where
    eqp (InL x) (InL y) = eqp x y
    eqp (InR x) (InR y) = eqp x y
    eqp _ _ = False

instance (EqP a, EqP b) => EqP (Product a b) where
    eqp (Pair x y) (Pair x' y') = eqp x x' && eqp y y'
#endif

instance (EqP f, EqP g) => EqP (f :+: g) where
    eqp (L1 x) (L1 y) = eqp x y
    eqp (R1 x) (R1 y) = eqp x y
    eqp _ _ = False

instance (EqP a, EqP b) => EqP (a :*: b) where
    eqp (x :*: y) (x' :*: y') = eqp x x' && eqp y y'

instance EqP TR.TypeRep where
    eqp x y = TR.SomeTypeRep x == TR.SomeTypeRep y

#if MIN_VERSION_base(4,18,0)
instance EqP TL.SChar where
    eqp x y = TL.fromSChar x == TL.fromSChar y

instance EqP TL.SSymbol where
    eqp x y = TL.fromSSymbol x == TL.fromSSymbol y

instance EqP TN.SNat where
    eqp x y = TN.fromSNat x == TN.fromSNat y
#endif

instance EqP Proxy where
    eqp _ _ = True

instance Eq a => EqP (Const a) where
    eqp (Const x) (Const y) = x == y

instance EqP StableName where
    eqp = eqStableName
