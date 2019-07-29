{-# LANGUAGE CPP                 #-}
#if __GLASGOW_HASKELL_ >=708
#error "Trying to compiled Data.Type.Equality module with GHC-7.8+"
#endif
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
-- only Trustworthy, as we have manual Typeable instance
{-# LANGUAGE Trustworthy         #-}
#endif
module Data.Type.Equality (
    -- * Type equality
    (:~:) (..),
    -- * Combinators
    sym,
    trans,
    castWith,
    gcastWith,

#if __GLASGOW_HASKELL__ >= 706
    apply,
#endif

    -- * TestEquality
    -- 
    -- | Provided only for GHC-7.6.
    -- Also there isn't @==@ type family, as it requires close type families,
    -- which are available only since GHC-7.8.
    TestEquality (..),
    ) where

import qualified Control.Category as C
import Data.Typeable (Typeable2 (..), Typeable)
import Data.Data (Data (..), Constr, mkConstr, Fixity(Prefix), DataType, mkDataType, mkTyConApp, TyCon )

#if MIN_VERSION_base(4,4,0)
import Data.Data (mkTyCon3)
#else
import Data.Data (mkTyCon)
#endif

#ifndef CURRENT_PACKAGE_KEY
import Data.Version (showVersion)
import Paths_type_equality_compat (version)
#endif

-- | Propositional equality. If @a ':~:' b@ is inhabited by some
-- terminating value, then the type @a@ is the same as the type
-- @b@. To use this equality in practice, pattern-match on the
-- @a ':~:' b@ to get out the 'Refl' constructor; in the body of
-- the pattern-match, the compiler knows that @a ~ b@.
--
-- /Note:/ this definition is polykinded only since GHC-7.6.
--
data a :~: b where
    Refl :: a :~: a

deriving instance a ~ b => Read (a :~: b)
deriving instance Show (a :~: b)

instance C.Category (:~:) where
    id = Refl
    Refl . Refl = Refl

instance Eq (a :~: b) where
    _ == _ = True

instance Ord (a :~: b) where
    compare _ _ = EQ

instance a ~ b => Enum (a :~: b) where
    toEnum 0 = Refl
    toEnum _ = error "Data.Type.Equality.toEnum: bad argument"

    fromEnum Refl = 0

instance a ~ b => Bounded (a :~: b) where
    minBound = Refl
    maxBound = Refl

-------------------------------------------------------------------------------
-- Typeable & Data
-------------------------------------------------------------------------------

instance Typeable2 (:~:) where
  typeOf2 t = mkTyConApp eqTyCon []


typeEqualityCompatPackageKey :: String
#ifdef CURRENT_PACKAGE_KEY
typeEqualityCompatPackageKey = CURRENT_PACKAGE_KEY
#else
typeEqualityCompatPackageKey = "type-equality-compat-" ++ showVersion version
#endif

eqTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
eqTyCon = mkTyCon3 typeEqualityCompatPackageKey "Data.Type.Equality" ":~:"
#else
eqTyCon = mkTyCon "Data.Type.Equality.:~:"
#endif

instance (Typeable a, Typeable b, a ~ b) => Data (a :~: b) where
  gfoldl _ z Refl = z Refl
  gunfold _ z _ = z Refl
  toConstr Refl = reflConstr
  dataTypeOf _ = eqDataType

eqDataType :: DataType
eqDataType = mkDataType ":~:" [reflConstr]

reflConstr :: Constr
reflConstr = mkConstr eqDataType "Refl" [] Prefix

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | Symmetry of equality
sym :: (a :~: b) -> (b :~: a)
sym Refl = Refl

-- | Transitivity of equality
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
trans Refl Refl = Refl

-- | Type-safe cast, using propositional equality
castWith :: (a :~: b) -> a -> b
castWith Refl x = x

-- | Generalized form of type-safe cast using propositional equality
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

#if __GLASGOW_HASKELL__ >= 706
-- | Apply one equality to another, respectively
apply :: (f :~: g) -> (a :~: b) -> (f a :~: g b)
apply Refl Refl = Refl
#endif

-------------------------------------------------------------------------------
-- TestEquality
-------------------------------------------------------------------------------

-- | This class contains types where you can learn the equality of two types
-- from information contained in /terms/. Typically, only singleton types should
-- inhabit this class.
class TestEquality f where
  -- | Conditionally prove the equality of @a@ and @b@.
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality ((:~:) a) where
  testEquality Refl Refl = Just Refl
