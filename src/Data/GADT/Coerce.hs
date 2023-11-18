{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds           #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module Data.GADT.Coerce (
    GCoercible (..),
    defaultGcoercible,
    ) where

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.IORef           (IORef)
import Data.STRef           (STRef)
import Data.Type.Equality   ((:~:) (..))
import Data.Type.Coercion

import Unsafe.Coerce        (unsafeCoerce)

import Data.GADT.Compare    (GEq, geq)

#if MIN_VERSION_base(4,10,0)
import           Data.Type.Equality ((:~~:) (..))
import qualified Type.Reflection    as TR
#endif

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type, Constraint)
#endif

-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the coercibility of types
-- occurring within them.
#if __GLASGOW_HASKELL__ >= 810
type GCoercible :: (k -> Type) -> Constraint
#endif
class GCoercible f where
    gcoercible :: f a -> f b -> Maybe (Coercion a b)

-- |If 'f' has a 'GEq' instance, this function makes a suitable default
-- implementation of 'gcoercible'.
defaultGcoercible :: GEq f => f a -> f b -> Maybe (Coercion a b)
defaultGcoercible x y = fmap repr $ geq x y

instance GCoercible ((:~:) a) where
    gcoercible = defaultGcoercible

#if MIN_VERSION_base(4,10,0)
instance GCoercible ((:~~:) a) where
    gcoercible = defaultGcoercible

instance GCoercible TR.TypeRep where
    gcoercible = defaultGcoercible
#endif

instance (GCoercible a, GCoercible b) => GCoercible (Sum a b) where
    gcoercible (InL x) (InL y) = gcoercible x y
    gcoercible (InR x) (InR y) = gcoercible x y
    gcoercible _ _ = Nothing

instance (GCoercible a, GCoercible b) => GCoercible (Product a b) where
    gcoercible (Pair x y) (Pair x' y') = do
        Coercion <- gcoercible x x'
        Coercion <- gcoercible y y'
        return Coercion

instance GCoercible IORef where
    gcoercible x y =
        if x == unsafeCoerce y
        then Just $ unsafeCoerce $ repr Refl
        else Nothing

instance GCoercible (STRef s) where
    gcoercible x y =
        if x == unsafeCoerce y
        then Just $ unsafeCoerce $ repr Refl
        else Nothing

-- This instance seems nice, but it's simply not right:
--
-- > instance GCoercible StableName where
-- >     gcoercible sn1 sn2
-- >         | sn1 == unsafeCoerce sn2
-- >             = Just (unsafeCoerce Refl)
-- >         | otherwise     = Nothing
--
-- Proof:
--
-- > x <- makeStableName id :: IO (StableName (Int -> Int))
-- > y <- makeStableName id :: IO (StableName ((Int -> Int) -> Int -> Int))
-- >
-- > let Just boom = gcoercible x y
-- >
-- > Data.Type.Coercion.coerceWith boom (const 0) id 0
-- > let "Illegal Instruction" = "QED."
--
-- The core of the problem is that 'makeStableName' only knows the closure it is
-- passed to, not any type information. Together with the fact that the same
-- closure has the same 'StableName' each time 'makeStableName' is called on it,
-- there is potential for abuse when a closure can be given many incompatible
-- types.
--
-- 'GCoericble' gets us closer than GEq, but the problem is Coercions state that
-- *all* values can be coerced, but due to polymophism it is quite easy to find
-- situations where some values of a type are safe to coerce and others are not.
-- We just need one such value to abuse 'GCoercible StableName'.
