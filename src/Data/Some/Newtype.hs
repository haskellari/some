{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE PatternSynonyms #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds       #-}
#endif
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy     #-}
#endif
module Data.Some.Newtype (
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
    ThenSome (..),
    BeforeSome (..)
    ) where

import Control.Applicative (Applicative (..))
import Control.DeepSeq     (NFData (..))
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce         (coerce)
#endif
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import GHC.Exts            (Any)
import Unsafe.Coerce       (unsafeCoerce)

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
-- You can either use @PatternSynonyms@ (available with GHC >= 8.0)
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
newtype Some tag = UnsafeSome (tag Any)
 
#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE Some #-}
pattern Some :: tag a -> Some tag
#if __GLASGOW_HASKELL__ >= 802
pattern Some x <- UnsafeSome x
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)
#else
-- There was a bug type checking pattern synonyms that prevented the
-- obvious thing from working.
pattern Some x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)
#endif
#endif

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome = \x -> UnsafeSome (unsafeCoerce x)

-- | Eliminator.
withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (UnsafeSome thing) some = some thing

-- | Eliminator.
usingSome :: forall tag b. (forall a. tag a -> b) -> Some tag -> b
usingSome some = coerce (some :: tag Any -> b)

-- | Monadic 'withSome'.
--
-- @since 1.0.1
withSomeM :: Monad m => m (Some tag) -> (forall a. tag a -> m r) -> m r
withSomeM m k = m >>= \s -> withSome s k

-- | @'flip' 'withSome'@
foldSome :: (forall a. tag a -> b) -> Some tag -> b
foldSome some (UnsafeSome thing) = some thing

-- | Map over argument.
mapSome :: (forall t. f t -> g t) -> Some f -> Some g
mapSome f (UnsafeSome x) = UnsafeSome (f x)

-- | Traverse over argument.
traverseSome :: Functor m => (forall a. f a -> m (g a)) -> Some f -> m (Some g)
traverseSome f x = withSome x $ \x' -> fmap mkSome (f x')

instance GShow tag => Show (Some tag) where
    showsPrec p some = withSome some $ \thing -> showParen (p > 10)
        ( showString "Some "
        . gshowsPrec 11 thing
        )

instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (getGReadResult withTag mkSome, rest')
        | (con, rest) <- lex s
        , con == "Some" || con == "mkSome"
        , (withTag, rest') <- greadsPrec 11 rest
        ]

instance GEq tag => Eq (Some tag) where
    (==) = coerce (defaultEq :: tag Any -> tag Any -> Bool)

instance GCompare tag => Ord (Some tag) where
    compare = coerce (defaultCompare :: tag Any -> tag Any -> Ordering)

instance GNFData tag => NFData (Some tag) where
    rnf = usingSome grnf

instance Control.Applicative.Applicative m => Data.Semigroup.Semigroup (Some m) where
    (<>) = coerce ((<>) :: ThenSome m -> ThenSome m -> ThenSome m)

instance Applicative m => Data.Monoid.Monoid (Some m) where
    mempty = getThenSome mempty
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

-- | A 'Monoid' using '*>' for an underlying 'Applicative' functor.
-- This has the same 'Monoid' instance as 'Some'.
newtype ThenSome f = ThenSome { getThenSome :: Some f }
deriving instance GEq f => Eq (ThenSome f)
deriving instance GCompare f => Ord (ThenSome f)
deriving instance GRead f => Read (ThenSome f)
deriving instance GShow f => Show (ThenSome f)

-- This should really be Apply, but we can't do that for now.
instance Applicative f => Semigroup (ThenSome f) where
    (<>) = coerce ((*>) :: f Any -> f Any -> f Any)

instance Applicative f => Monoid (ThenSome f) where
  mempty = ThenSome (mkSome (pure ()))
#if !MIN_VERSION_base(4,11,0)
  mappend = coerce ((*>) :: f Any -> f Any -> f Any)
#endif

-- | A 'Monoid' using '<*' for an underlying 'Applicative' functor.
newtype BeforeSome f = BeforeSome { getBeforeSome :: Some f }
deriving instance GEq f => Eq (BeforeSome f)
deriving instance GCompare f => Ord (BeforeSome f)
deriving instance GRead f => Read (BeforeSome f)
deriving instance GShow f => Show (BeforeSome f)

-- This should really be Apply, but we can't do that for now.
instance Applicative f => Semigroup (BeforeSome f) where
  (<>) = coerce ((<*) :: f Any -> f Any -> f Any)

instance Applicative f => Monoid (BeforeSome f) where
  mempty = BeforeSome (mkSome (pure ()))
#if !MIN_VERSION_base(4,11,0)
  -- Some older GHC versions choke on mappend = (<>)
  -- because of some sort of constraint-resolution bug.
  mappend = coerce ((<*) :: f Any -> f Any -> f Any)
#endif


#if __GLASGOW_HASKELL__ < 708
coerce :: a -> b
coerce x = unsafeCoerce x
#endif
