{-# LANGUAGE CPP         #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE PolyKinds   #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
{-# LANGUAGE Safe        #-}
module Data.GADT.DeepSeq (
    GNFData (..),
    ) where

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Type.Equality   ((:~:) (..))

#if MIN_VERSION_base(4,6,0)
import GHC.Generics         ((:+:) (..), (:*:) (..))
#endif

#if MIN_VERSION_base(4,9,0)
#if MIN_VERSION_base(4,10,0)
import  Data.Type.Equality ((:~~:) (..))
#else
import  Data.Type.Equality.Hetero ((:~~:) (..))
#endif
#endif

#if MIN_VERSION_base(4,10,0)
import qualified Type.Reflection    as TR
#endif

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type, Constraint)
#endif

#if __GLASGOW_HASKELL__ >= 810
type GNFData :: (k -> Type) -> Constraint
#endif

class GNFData f where
    grnf :: f a -> ()

instance (GNFData a, GNFData b) => GNFData (Product a b) where
    grnf (Pair a b) = grnf a `seq` grnf b

instance (GNFData a, GNFData b) => GNFData (Sum a b) where
    grnf (InL x) = grnf x
    grnf (InR y) = grnf y

#if MIN_VERSION_base(4,6,0)
instance (GNFData a, GNFData b) => GNFData (a :*: b) where
    grnf (a :*: b) = grnf a `seq` grnf b

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
    grnf (L1 x) = grnf x
    grnf (R1 y) = grnf y
#endif

-- | @since 1.0.3
instance GNFData ((:~:) a) where
    grnf Refl = ()

#if MIN_VERSION_base(4,9,0)
-- | @since 1.0.4
instance GNFData ((:~~:) a) where
    grnf HRefl = ()
#endif

#if MIN_VERSION_base(4,10,0)
-- | @since 1.0.3
instance GNFData TR.TypeRep where
    grnf = TR.rnfTypeRep
#endif
