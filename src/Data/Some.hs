{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | An existential type.
--
-- The constructor is exported only on GHC-8 and later.
module Data.Some (
#if __GLASGOW_HASKELL__ >= 801
Some(Some),
#else
Some,
#endif
mkSome,
withSome,
withSomeM,
mapSome,
foldSome,
traverseSome,
) where

#ifdef SOME_NEWTYPE
import Data.Some.Newtype
#else
import Data.Some.GADT
#endif
