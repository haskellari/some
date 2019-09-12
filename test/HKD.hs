{-# LANGUAGE CPP           #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Applicative as A
import Data.Monoid as Mon

-- from Some package
import qualified Data.Some.Church  as C
import qualified Data.Some.GADT    as G
import qualified Data.Some.Newtype as N

class FFoldable t where
  ffoldMap :: Monoid m => (forall a. f a -> m) -> t f -> m

-- one derived operation, is ftraverse_, which we can implement using ffoldMap

gadt_ftraverse_ :: (FFoldable t, A.Applicative m) => (forall a. f a -> m b) -> t f -> m ()
gadt_ftraverse_ k tf = case ffoldMap (G.Some . k) tf of
  G.Some mx -> () <$ mx

newtype_ftraverse_ :: (FFoldable t, Applicative m) => (forall a. f a -> m b) -> t f -> m ()
#if __GLASGOW_HASKELL__ >= 801
newtype_ftraverse_ k tf = case ffoldMap (N.Some . k) tf of
  N.Some mx -> () <$ mx
#else
newtype_ftraverse_ k tf = N.withSome (ffoldMap (N.mkSome . k) tf) $
  \mx -> () <$ mx
#endif

church_ftraverse_ :: (FFoldable t, Applicative m) => (forall a. f a -> m b) -> t f -> m ()
church_ftraverse_ k tf = C.withSome (ffoldMap (C.mkSome . k) tf) $
  \mx -> () <$ mx

-- ghc -c -fforce-recomp -O -ddump-simpl -dsuppress-all HKD.hs

data Ex f where
    Nil :: Ex f
    Cons :: f a -> Ex f -> Ex f

instance FFoldable Ex where
    ffoldMap f = go where
        go Nil         = Mon.mempty
        go (Cons x xs) = mappend (f x) (go xs)

gadt_ftraverse_Ex :: Applicative m => (forall a. f a -> m b) -> Ex f -> m ()
gadt_ftraverse_Ex = gadt_ftraverse_

newtype_ftraverse_Ex :: Applicative m => (forall a. f a -> m b) -> Ex f -> m ()
newtype_ftraverse_Ex = newtype_ftraverse_

church_ftraverse_Ex :: Applicative m => (forall a. f a -> m b) -> Ex f -> m ()
church_ftraverse_Ex = church_ftraverse_

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = return ()
