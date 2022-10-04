{-# LANGUAGE CPP         #-}
{-# LANGUAGE Safe        #-}
-- | An existential type.
--
-- The constructor is exported only on GHC-8 and later.
module Data.Some (
Some(Some),
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
