{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
#define GHC __GLASGOW_HASKELL__
#if (GHC >= 704 && GHC <707) || GHC >= 801
{-# LANGUAGE Safe                #-}
#else
{-# LANGUAGE Trustworthy         #-}
#endif
#undef GH
#endif
module Data.GADT.Compare (
    -- * Equality
    GEq (..),
    defaultEq,
    defaultNeq,
    -- * Total order comparison
    GCompare (..),
    defaultCompare,
    GOrdering (..),
    ) where

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.GADT.Show
import Data.Maybe           (isJust, isNothing)
import Data.Type.Equality   ((:~:) (..))

#if __GLASGOW_HASKELL__ >=708
import Data.Typeable        (Typeable)
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Type.Equality (testEquality)
import qualified Type.Reflection    as TR
#endif


-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the equality of types
-- occurring within them.
class GEq f where
    -- |Produce a witness of type-equality, if one exists.
    --
    -- A handy idiom for using this would be to pattern-bind in the Maybe monad, eg.:
    --
    -- > extract :: GEq tag => tag a -> DSum tag -> Maybe a
    -- > extract t1 (t2 :=> x) = do
    -- >     Refl <- geq t1 t2
    -- >     return x
    --
    -- Or in a list comprehension:
    --
    -- > extractMany :: GEq tag => tag a -> [DSum tag] -> [a]
    -- > extractMany t1 things = [ x | (t2 :=> x) <- things, Refl <- maybeToList (geq t1 t2)]
    --
    -- (Making use of the 'DSum' type from "Data.Dependent.Sum" in both examples)
    geq :: f a -> f b -> Maybe (a :~: b)

-- |If 'f' has a 'GEq' instance, this function makes a suitable default
-- implementation of '(==)'.
defaultEq :: GEq f => f a -> f b -> Bool
defaultEq x y = isJust (geq x y)

-- |If 'f' has a 'GEq' instance, this function makes a suitable default
-- implementation of '(/=)'.
defaultNeq :: GEq f => f a -> f b -> Bool
defaultNeq x y = isNothing (geq x y)

instance GEq ((:~:) a) where
    geq (Refl :: a :~: b) (Refl :: a :~: c) = Just (Refl :: b :~: c)

instance (GEq a, GEq b) => GEq (Sum a b) where
    geq (InL x) (InL y) = geq x y
    geq (InR x) (InR y) = geq x y
    geq _ _ = Nothing

instance (GEq a, GEq b) => GEq (Product a b) where
    geq (Pair x y) (Pair x' y') = do
        Refl <- geq x x'
        Refl <- geq y y'
        return Refl

#if MIN_VERSION_base(4,10,0)
instance GEq TR.TypeRep where
    geq = testEquality
#endif

-- This instance seems nice, but it's simply not right:
--
-- > instance GEq StableName where
-- >     geq sn1 sn2
-- >         | sn1 == unsafeCoerce sn2
-- >             = Just (unsafeCoerce Refl)
-- >         | otherwise     = Nothing
--
-- Proof:
--
-- > x <- makeStableName id :: IO (StableName (Int -> Int))
-- > y <- makeStableName id :: IO (StableName ((Int -> Int) -> Int -> Int))
-- >
-- > let Just boom = geq x y
-- > let coerce :: (a :~: b) -> a -> b; coerce Refl = id
-- >
-- > coerce boom (const 0) id 0
-- > let "Illegal Instruction" = "QED."
--
-- The core of the problem is that 'makeStableName' only knows the closure
-- it is passed to, not any type information.  Together with the fact that
-- the same closure has the same StableName each time 'makeStableName' is
-- called on it, there is serious potential for abuse when a closure can
-- be given many incompatible types.


-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where
-- they are equal their parameter types can be unified.
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b
#if __GLASGOW_HASKELL__ >=708
  deriving Typeable
#endif

-- |TODO: Think of a better name
--
-- This operation forgets the phantom types of a 'GOrdering' value.
weakenOrdering :: GOrdering a b -> Ordering
weakenOrdering GLT = LT
weakenOrdering GEQ = EQ
weakenOrdering GGT = GT

instance Eq (GOrdering a b) where
    x == y = weakenOrdering x == weakenOrdering y

instance Ord (GOrdering a b) where
    compare x y = compare (weakenOrdering x) (weakenOrdering y)

instance Show (GOrdering a b) where
    showsPrec _ GGT = showString "GGT"
    showsPrec _ GEQ = showString "GEQ"
    showsPrec _ GLT = showString "GLT"

instance GShow (GOrdering a) where
    gshowsPrec = showsPrec

instance GRead (GOrdering a) where
    greadsPrec _ s = case con of
        "GGT"   -> [(GReadResult (\x -> x GGT), rest)]
        "GEQ"   -> [(GReadResult (\x -> x GEQ), rest)]
        "GLT"   -> [(GReadResult (\x -> x GLT), rest)]
        _       -> []
        where (con, rest) = splitAt 3 s

-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well ('GEQ').
class GEq f => GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

instance GCompare ((:~:) a) where
    gcompare Refl Refl = GEQ

#if MIN_VERSION_base(4,10,0)
instance GCompare TR.TypeRep where
    gcompare t1 t2 =
      case testEquality t1 t2 of
        Just Refl -> GEQ
        Nothing ->
          case compare (TR.SomeTypeRep t1) (TR.SomeTypeRep t2) of
            LT -> GLT
            GT -> GGT
            EQ -> error "impossible: 'testEquality' and 'compare' \
                        \are inconsistent for TypeRep; report this \
                        \as a GHC bug"
#endif

defaultCompare :: GCompare f => f a -> f b -> Ordering
defaultCompare x y = weakenOrdering (gcompare x y)

instance (GCompare a, GCompare b) => GCompare (Sum a b) where
    gcompare (InL x) (InL y) = gcompare x y
    gcompare (InL _) (InR _) = GLT
    gcompare (InR _) (InL _) = GGT
    gcompare (InR x) (InR y) = gcompare x y

instance (GCompare a, GCompare b) => GCompare (Product a b) where
    gcompare (Pair x y) (Pair x' y') = case gcompare x x' of
        GLT -> GLT
        GGT -> GGT
        GEQ -> case gcompare y y' of
            GLT -> GLT
            GEQ -> GEQ
            GGT -> GGT
