{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
#if __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
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
    mapSome,
    ) where

import Data.GADT.Compare
import Data.GADT.Show
import GHC.Exts          (Any)
import Unsafe.Coerce     (unsafeCoerce)

-- $setup
-- >>> :set -XKindSignatures -XGADTs

-- | Existential. This is type is useful to hide GADTs' parameters.
--
-- >>> data Tag :: * -> * where TagInt :: Tag Int; TagBool :: Tag Bool
-- >>> instance GShow Tag where gshowsPrec _ TagInt = showString "TagInt"; gshowsPrec _ TagBool = showString "TagBool"
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
newtype Some tag = UnsafeSome (tag Any)

#if __GLASGOW_HASKELL__ >= 801
{-# COMPLETE Some #-}
pattern Some :: tag a -> Some tag
pattern Some x <- UnsafeSome ((unsafeCoerce :: tag Any -> tag a) -> x)
  where Some x = UnsafeSome ((unsafeCoerce :: tag a -> tag Any) x)
#endif

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome = UnsafeSome . unsafeCoerce

-- | Eliminator.
withSome :: Some tag -> (forall a. tag a -> b) -> b
withSome (UnsafeSome thing) some = some (unsafeCoerce thing)

instance GShow tag => Show (Some tag) where
    showsPrec p some = withSome some $ \thing -> showParen (p > 10)
        ( showString "Some "
        . gshowsPrec 11 thing
        )

instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (getGReadResult withTag mkSome, rest')
        | let (con, rest) = splitAt 5 s
        , con == "Some "
        , (withTag, rest') <- greadsPrec 11 rest
        ]

instance GEq tag => Eq (Some tag) where
    x == y =
        withSome x $ \x' ->
        withSome y $ \y' -> defaultEq x' y'

instance GCompare tag => Ord (Some tag) where
    compare x y =
        withSome x $ \x' ->
        withSome y $ \y' -> defaultCompare x' y'

mapSome :: (forall t. f t -> g t) -> Some f -> Some g
mapSome f (UnsafeSome x) = UnsafeSome (unsafeCoerce f x)
