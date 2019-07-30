{-# LANGUAGE CPP                 #-}
#if __GLASGOW_HASKELL__ <800
#error "Trying to compiled Data.Type.Equality.Hetero module with GHC <7.6"
#endif
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE Trustworthy         #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE TypeOperators       #-}
-- | This module shims kind heterogeneous propositional equality.
--
-- Note: some instances: 'Read', 'Enum', 'Bounded' and 'Data' would be available
-- only since GHC-8.0 as we need @~~@ constraint.
-- Also GH-7.10.3 is nitpicky /data constructor ‘HRefl’ cannot be GADT-like in its *kind* arguments/,
-- thus this module is available only with GHC >= 8.0
module Data.Type.Equality.Hetero (
    (:~~:)(..),
    ) where

#if MIN_VERSION_base(4,10,0)
import Data.Type.Equality ((:~~:)(..))
#else

import qualified Control.Category as C
import Data.Data (Data)
import Data.Type.Equality
import Data.Typeable (Typeable)

#if MIN_VERSION_base(4,7,0)
import Data.Type.Coercion (TestCoercion (..), Coercion (..))
#endif

-- | Kind heterogeneous propositional equality. Like ':~:', @a :~~: b@ is
-- inhabited by a terminating value if and only if @a@ is the same type as @b@.
data (a :: k1) :~~: (b :: k2) where
   HRefl :: a :~~: a

infixr 4 :~~:

deriving instance Eq   (a :~~: b)
deriving instance Show (a :~~: b)
deriving instance Ord  (a :~~: b)

deriving instance a ~~ b => Read (a :~~: b)

instance a ~~ b => Enum (a :~~: b) where
  toEnum 0 = HRefl
  toEnum _ = errorWithoutStackTrace "Data.Type.Equality.Hetero.toEnum: bad argument"

  fromEnum HRefl = 0

deriving instance a ~~ b => Bounded (a :~~: b)

deriving instance Typeable (:~~:)
deriving instance (Typeable i, Typeable j, Typeable a, Typeable b,
                    (a :: i) ~~ (b :: j)) => Data (a :~~: b)

instance C.Category (:~~:) where
    id = HRefl
    HRefl . HRefl = HRefl

instance TestEquality ((:~~:) a) where
  testEquality HRefl HRefl = Just Refl

instance TestCoercion ((:~~:) a) where
  testCoercion HRefl HRefl = Just Coercion
#endif
