{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
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

import Data.GADT.Internal
