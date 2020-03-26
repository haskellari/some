{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.GADT.DeepSeq (
    GNFData (..),
    ) where

import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))

class GNFData f where
    grnf :: f a -> ()

instance (GNFData a, GNFData b) => GNFData (Product a b) where
    grnf (Pair a b) = grnf a `seq` grnf b

instance (GNFData a, GNFData b) => GNFData (Sum a b) where
    grnf (InL x) = grnf x
    grnf (InR y) = grnf y
