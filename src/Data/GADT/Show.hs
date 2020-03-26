{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.GADT.Show (
    -- * Showing
    GShow  (..),
    gshows,
    gshow,
    -- * Reading
    GRead  (..),
    GReadS,
    greads,
    gread,
    greadMaybe,
    getGReadResult,
    mkGReadResult,
    ) where

import Data.GADT.Internal
