{-# LANGUAGE CPP         #-}
{-# LANGUAGE Safe        #-}
module Data.Some.Church (
    Some(..),
    mkSome,
    mapSome,
    withSomeM,
    foldSome,
    traverseSome,
    ) where

import Data.GADT.Internal
