{-# LANGUAGE CPP #-}
#ifdef SOME_NEWTYPE
module Data.Some (module Data.Some.Newtype) where
import Data.Some.Newtype
#else
module Data.Some (module Data.Some.GADT) where
import Data.Some.GADT
#endif
