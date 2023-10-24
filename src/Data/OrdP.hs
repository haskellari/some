{-# LANGUAGE CPP                   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module Data.OrdP (
    OrdP (..),
) where

import Control.Applicative  (Const (..))
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))
import Data.Semigroup       ((<>))
import Data.Type.Equality   ((:~:) (..), (:~~:) (..))
import GHC.Generics         ((:*:) (..), (:+:) (..))

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

import Data.EqP

-- | Heterogenous lifted total order.
--
-- This class is stronger version of 'Ord1' from @base@
--
-- @
-- class (forall a. Ord a => Ord (f a)) => Ord1 f where
--     liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering
-- @
--
-- @since 1.0.5
#if __GLASGOW_HASKELL__ >= 810
type OrdP :: (k -> Type) -> Constraint
#endif
class (EqP f, forall a. Ord (f a)) => OrdP (f :: k -> Type) where
    comparep :: f a -> f b -> Ordering

instance OrdP ((:~:) a) where
    comparep _ _ = EQ

instance OrdP ((:~~:) a) where
    comparep _ _ = EQ

#if MIN_VERSION_base(4,18,0)
instance (OrdP a, OrdP b) => OrdP (Sum a b) where
    comparep (InL x) (InL y) = comparep x y
    comparep (InL _) (InR _) = LT
    comparep (InR x) (InR y) = comparep x y
    comparep (InR _) (InL _) = GT

instance (OrdP a, OrdP b) => OrdP (Product a b) where
    comparep (Pair x y) (Pair x' y') = comparep x x' <> comparep y y'
#endif

instance (OrdP f, OrdP g) => OrdP (f :+: g) where
    comparep (L1 x) (L1 y) = comparep x y
    comparep (L1 _) (R1 _) = LT
    comparep (R1 x) (R1 y) = comparep x y
    comparep (R1 _) (L1 _) = GT

instance (OrdP a, OrdP b) => OrdP (a :*: b) where
    comparep (x :*: y) (x' :*: y') = comparep x x' <> comparep y y'

instance OrdP TR.TypeRep where
    comparep x y = compare (TR.SomeTypeRep x) (TR.SomeTypeRep y)

#if MIN_VERSION_base(4,18,0)
instance OrdP TL.SChar where
    comparep x y = compare (TL.fromSChar x) (TL.fromSChar y)

instance OrdP TL.SSymbol where
    comparep x y = compare (TL.fromSSymbol x) (TL.fromSSymbol y)

instance OrdP TN.SNat where
    comparep x y = compare (TN.fromSNat x) (TN.fromSNat y)
#endif

instance OrdP Proxy where
    comparep _ _ = EQ

instance Ord a => OrdP (Const a) where
    comparep (Const x) (Const y) = compare x y
