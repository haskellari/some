{-# LANGUAGE CPP         #-}
{-# LANGUAGE Safe        #-}
module Data.GADT.Compare (
    -- * Equality
    GEq (..),
    defaultGeq,
    defaultEq,
    defaultNeq,
    -- * Total order comparison
    GCompare (..),
    defaultCompare,
    GOrdering (..),
    ) where

import Data.GADT.Internal
