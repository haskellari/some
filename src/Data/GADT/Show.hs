{-# LANGUAGE CPP         #-}
{-# LANGUAGE Safe        #-}
module Data.GADT.Show (
    -- * Showing
    GShow  (..),
    defaultGshowsPrec,
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
