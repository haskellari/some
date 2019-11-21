{-# LANGUAGE CPP         #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE RankNTypes  #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds   #-}
#endif
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Some.GADT (
    Some(Some),
    mkSome,
    withSome,
    withSomeM,
    mapSome,
    foldSome,
    traverseSome,
    ) where

import Control.Applicative (Applicative (..))
import Control.DeepSeq     (NFData (..))
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))

import Data.GADT.Compare
import Data.GADT.DeepSeq
import Data.GADT.Show

-- $setup
-- >>> :set -XKindSignatures -XGADTs

-- | Existential. This is type is useful to hide GADTs' parameters.
--
-- >>> data Tag :: * -> * where TagInt :: Tag Int; TagBool :: Tag Bool
-- >>> instance GShow Tag where gshowsPrec _ TagInt = showString "TagInt"; gshowsPrec _ TagBool = showString "TagBool"
-- >>> classify s = case s of "TagInt" -> [mkGReadResult TagInt]; "TagBool" -> [mkGReadResult TagBool]; _ -> []
-- >>> instance GRead Tag where greadsPrec _ s = [ (r, rest) | (con, rest) <-  lex s, r <- classify con ]
--
-- You can either use constructor:
--
-- >>> let x = Some TagInt
-- >>> x
-- Some TagInt
--
-- >>> case x of { Some TagInt -> "I"; Some TagBool -> "B" } :: String
-- "I"
--
-- or you can use functions
--
-- >>> let y = mkSome TagBool
-- >>> y
-- Some TagBool
--
-- >>> withSome y $ \y' -> case y' of { TagInt -> "I"; TagBool -> "B" } :: String
-- "B"
--
-- The implementation of 'mapSome' is /safe/.
--
-- >>> let f :: Tag a -> Tag a; f TagInt = TagInt; f TagBool = TagBool
-- >>> mapSome f y
-- Some TagBool
--
-- but you can also use:
--
-- >>> withSome y (mkSome . f)
-- Some TagBool
--
-- >>> read "Some TagBool" :: Some Tag
-- Some TagBool
--
-- >>> read "mkSome TagInt" :: Some Tag
-- Some TagInt
--
data Some tag where
    Some :: tag a -> Some tag

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome = Some

-- | Eliminator.
withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (Some thing) some = some thing

-- | Monadic 'withSome'.
--
-- @since 1.0.1
withSomeM :: Monad m => m (Some tag) -> (forall a. tag a -> m r) -> m r
withSomeM m k = m >>= \s -> withSome s k

-- | @'flip' 'withSome'@
foldSome :: (forall a. tag a -> b) -> Some tag -> b
foldSome some (Some thing) = some thing

-- | Map over argument.
mapSome :: (forall x. f x -> g x) ->  Some f -> Some g
mapSome nt (Some fx) = Some (nt fx)

-- | Traverse over argument.
traverseSome :: Functor m => (forall a. f a -> m (g a)) -> Some f -> m (Some g)
traverseSome f (Some x) = fmap Some (f x)

instance GShow tag => Show (Some tag) where
    showsPrec p (Some thing) = showParen (p > 10)
        $ showString "Some "
        . gshowsPrec 11 thing

-- |
instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (getGReadResult withTag Some, rest')
        | (con, rest) <- lex s
        , con == "Some" || con == "mkSome"
        , (withTag, rest') <- greadsPrec 11 rest
        ]

instance GEq tag => Eq (Some tag) where
    Some x == Some y = defaultEq x y

instance GCompare tag => Ord (Some tag) where
    compare (Some x) (Some y) = defaultCompare x y

instance GNFData tag => NFData (Some tag) where
    rnf (Some x) = grnf x

instance Control.Applicative.Applicative m => Data.Semigroup.Semigroup (Some m) where
    Some m <> Some n = Some (m *> n)

instance Applicative m => Data.Monoid.Monoid (Some m) where
    mempty = Some (pure ())
    mappend = (<>)
