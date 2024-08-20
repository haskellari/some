{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GADT.Instances where

import Data.IORef          (IORef)
import Data.STRef          (STRef)
import Unsafe.Coerce       (unsafeCoerce)

import Data.GADT.Compare   (GEq(..))

#if __GLASGOW_HASKELL__ >=708
import Data.Typeable ((:~:)( Refl ))
#endif

instance GEq IORef where
  a `geq` b = if a == unsafeCoerce b then Just $ unsafeCoerce Refl else Nothing

instance GEq (STRef s) where
  a `geq` b = if a == unsafeCoerce b then Just $ unsafeCoerce Refl else Nothing

