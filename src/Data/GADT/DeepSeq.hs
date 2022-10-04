{-# LANGUAGE CPP                      #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE TypeOperators            #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module Data.GADT.DeepSeq (
    GNFData (..),
    ) where

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Type.Equality   ((:~:) (..), (:~~:) (..))
import GHC.Generics         ((:*:) (..), (:+:) (..))

import qualified Type.Reflection as TR

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint, Type)
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

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
    grnf (a :*: b) = grnf a `seq` grnf b

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
    grnf (L1 x) = grnf x
    grnf (R1 y) = grnf y

-- | @since 1.0.3
instance GNFData ((:~:) a) where
    grnf Refl = ()

-- | @since 1.0.4
instance GNFData ((:~~:) a) where
    grnf HRefl = ()

-- | @since 1.0.3
instance GNFData TR.TypeRep where
    grnf = TR.rnfTypeRep
