{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Some.Church (
    Some(..),
    mkSome,
    mapSome,
    withSomeM,
    foldSome,
    traverseSome,
    ) where

import Data.GADT.Internal
