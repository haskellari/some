{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 805
{-# LANGUAGE TypeInType #-}
#endif
#if (__GLASGOW_HASKELL__ >= 704 && __GLASGOW_HASKELL__ < 707) || __GLASGOW_HASKELL__ >= 801
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy         #-}
#endif
module Data.GADT.Internal where

import Control.Applicative  (Applicative (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Maybe           (isJust, isNothing)
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.Type.Equality   ((:~:) (..))
#if MIN_VERSION_base(4,6,0)
import GHC.Generics         ((:+:) (..), (:*:) (..))
#endif

#if __GLASGOW_HASKELL__ >=708
import Data.Typeable (Typeable)
#endif

#if MIN_VERSION_base(4,9,0)
#if MIN_VERSION_base(4,10,0)
import  Data.Type.Equality ((:~~:) (..))
#else
import  Data.Type.Equality.Hetero ((:~~:) (..))
#endif
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Type.Equality (testEquality)
import qualified Type.Reflection    as TR
#endif

#if __GLASGOW_HASKELL__ >= 800
import Data.Kind (Type)
#endif

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint)
#endif

-- $setup
-- >>> :set -XKindSignatures -XGADTs -XTypeOperators
-- >>> import Data.Type.Equality
-- >>> import Data.Functor.Sum
-- >>> import GHC.Generics

-- |'Show'-like class for 1-type-parameter GADTs.  @GShow t => ...@ is equivalent to something
-- like @(forall a. Show (t a)) => ...@.  The easiest way to create instances would probably be
-- to write (or derive) an @instance Show (T a)@, and then simply say:
--
-- > instance GShow t where gshowsPrec = defaultGshowsPrec
#if __GLASGOW_HASKELL__ >= 810
type GShow :: (k -> Type) -> Constraint
#endif
class GShow t where
    gshowsPrec :: Int -> t a -> ShowS

-- |If 'f' has a 'Show (f a)' instance, this function makes a suitable default
-- implementation of 'gshowsPrec'.
--
-- @since 1.0.4
defaultGshowsPrec :: Show (t a) => Int -> t a -> ShowS
defaultGshowsPrec = showsPrec

gshows :: GShow t => t a -> ShowS
gshows = gshowsPrec (-1)

gshow :: (GShow t) => t a -> String
gshow x = gshows x ""

instance GShow ((:~:) a) where
    gshowsPrec _ Refl = showString "Refl"

#if MIN_VERSION_base(4,9,0)
-- | @since 1.0.4
instance GShow ((:~~:) a) where
    gshowsPrec _ HRefl = showString "HRefl"
#endif

#if MIN_VERSION_base(4,10,0)
instance GShow TR.TypeRep where
    gshowsPrec = showsPrec
#endif

--
-- | >>> gshow (InL Refl :: Sum ((:~:) Int) ((:~:) Bool) Int)
-- "InL Refl"
instance (GShow a, GShow b) => GShow (Sum a b) where
    gshowsPrec d = \s -> case s of
        InL x -> showParen (d > 10) (showString "InL " . gshowsPrec 11 x)
        InR x -> showParen (d > 10) (showString "InR " . gshowsPrec 11 x)

-- | >>> gshow (Pair Refl Refl :: Product ((:~:) Int) ((:~:) Int) Int)
-- "Pair Refl Refl"
instance (GShow a, GShow b) => GShow (Product a b) where
    gshowsPrec d (Pair x y) = showParen (d > 10)
        $ showString "Pair "
        . gshowsPrec 11 x
        . showChar ' '
        . gshowsPrec 11 y

#if MIN_VERSION_base(4,6,0)
--
-- | >>> gshow (L1 Refl :: ((:~:) Int :+: (:~:) Bool) Int)
-- "L1 Refl"
--
-- @since 1.0.4
instance (GShow a, GShow b) => GShow (a :+: b) where
    gshowsPrec d = \s -> case s of
        L1 x -> showParen (d > 10) (showString "L1 " . gshowsPrec 11 x)
        R1 x -> showParen (d > 10) (showString "R1 " . gshowsPrec 11 x)

-- | >>> gshow (Pair Refl Refl :: Product ((:~:) Int) ((:~:) Int) Int)
-- "Refl :*: Refl"
--
-- @since 1.0.4
instance (GShow a, GShow b) => GShow (a :*: b) where
    gshowsPrec d (x :*: y) = showParen (d > 6)
        $ gshowsPrec 6 x
        . showString " :*: "
        . gshowsPrec 6 y
#endif

-- |@GReadS t@ is equivalent to @ReadS (forall b. (forall a. t a -> b) -> b)@, which is
-- in turn equivalent to @ReadS (Exists t)@ (with @data Exists t where Exists :: t a -> Exists t@)
#if __GLASGOW_HASKELL__ >= 810
type GReadS :: (k -> Type) -> Type
#endif
type GReadS t = String -> [(Some t, String)]

getGReadResult :: Some tag -> (forall a. tag a -> b) -> b
getGReadResult t k = withSome t k

mkGReadResult :: tag a -> Some tag
mkGReadResult = mkSome

-- |'Read'-like class for 1-type-parameter GADTs.  Unlike 'GShow', this one cannot be
-- mechanically derived from a 'Read' instance because 'greadsPrec' must choose the phantom
-- type based on the 'String' being parsed.
#if __GLASGOW_HASKELL__ >= 810
type GRead :: (k -> Type) -> Constraint
#endif
class GRead t where
    greadsPrec :: Int -> GReadS t

greads :: GRead t => GReadS t
greads = greadsPrec (-1)

gread :: GRead t => String -> (forall a. t a -> b) -> b
gread s g = withSome (hd [f | (f, "") <- greads s]) g where
    hd (x:_) = x
    hd _ = error "gread: no parse"

-- |
--
-- >>> greadMaybe "InL Refl" mkSome :: Maybe (Some (Sum ((:~:) Int) ((:~:) Bool)))
-- Just (mkSome (InL Refl))
--
-- >>> greadMaybe "L1 Refl" mkSome :: Maybe (Some ((:~:) Int :+: (:~:) Bool))
-- Just (mkSome (L1 Refl))
--
-- >>> greadMaybe "garbage" mkSome :: Maybe (Some ((:~:) Int))
-- Nothing
--
greadMaybe :: GRead t => String -> (forall a. t a -> b) -> Maybe b
greadMaybe s g = case [f | (f, "") <- greads s] of
    (x : _) -> Just (withSome x g)
    _       -> Nothing

instance GRead ((:~:) a) where
    greadsPrec _ = readParen False (\s ->
        [ (S $ \k -> k (Refl :: a :~: a), t)
        | ("Refl", t) <- lex s
        ])

#if MIN_VERSION_base(4,9,0)
-- | @since 1.0.4
instance k1 ~ k2 => GRead ((:~~:) (a :: k1) :: k2 -> Type) where
    greadsPrec _ = readParen False (\s ->
        [ (S $ \k -> k (HRefl :: a :~~: a), t)
        | ("HRefl", t) <- lex s
        ])
#endif

instance (GRead a, GRead b) => GRead (Sum a b) where
    greadsPrec d s =
        readParen (d > 10)
            (\s1 -> [ (S $ \k -> withSome r (k . InL), t)
                    | ("InL", s2) <- lex s1
                    , (r, t) <- greadsPrec 11 s2 ]) s
        ++
        readParen (d > 10)
            (\s1 -> [ (S $ \k -> withSome r (k . InR), t)
                    | ("InR", s2) <- lex s1
                    , (r, t) <- greadsPrec 11 s2 ]) s

#if MIN_VERSION_base(4,6,0)
-- | @since 1.0.4
instance (GRead a, GRead b) => GRead (a :+: b) where
    greadsPrec d s =
        readParen (d > 10)
            (\s1 -> [ (S $ \k -> withSome r (k . L1), t)
                    | ("L1", s2) <- lex s1
                    , (r, t) <- greadsPrec 11 s2 ]) s
        ++
        readParen (d > 10)
            (\s1 -> [ (S $ \k -> withSome r (k . R1), t)
                    | ("R1", s2) <- lex s1
                    , (r, t) <- greadsPrec 11 s2 ]) s
#endif

-------------------------------------------------------------------------------
-- GEq
-------------------------------------------------------------------------------

-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the equality of types
-- occurring within them.
#if __GLASGOW_HASKELL__ >= 810
type GEq :: (k -> Type) -> Constraint
#endif
class GEq f where
    -- |Produce a witness of type-equality, if one exists.
    --
    -- A handy idiom for using this would be to pattern-bind in the Maybe monad, eg.:
    --
    -- > extract :: GEq tag => tag a -> DSum tag -> Maybe a
    -- > extract t1 (t2 :=> x) = do
    -- >     Refl <- geq t1 t2
    -- >     return x
    --
    -- Or in a list comprehension:
    --
    -- > extractMany :: GEq tag => tag a -> [DSum tag] -> [a]
    -- > extractMany t1 things = [ x | (t2 :=> x) <- things, Refl <- maybeToList (geq t1 t2)]
    --
    -- (Making use of the 'DSum' type from <https://hackage.haskell.org/package/dependent-sum/docs/Data-Dependent-Sum.html Data.Dependent.Sum> in both examples)
    geq :: f a -> f b -> Maybe (a :~: b)

-- |If 'f' has a 'GCompare' instance, this function makes a suitable default
-- implementation of 'geq'.
--
-- @since 1.0.4
defaultGeq :: GCompare f => f a -> f b -> Maybe (a :~: b)
defaultGeq a b = case gcompare a b of
    GEQ -> Just Refl
    _   -> Nothing

-- |If 'f' has a 'GEq' instance, this function makes a suitable default
-- implementation of '(==)'.
defaultEq :: GEq f => f a -> f b -> Bool
defaultEq x y = isJust (geq x y)

-- |If 'f' has a 'GEq' instance, this function makes a suitable default
-- implementation of '(/=)'.
defaultNeq :: GEq f => f a -> f b -> Bool
defaultNeq x y = isNothing (geq x y)

instance GEq ((:~:) a) where
    geq (Refl :: a :~: b) (Refl :: a :~: c) = Just (Refl :: b :~: c)

#if MIN_VERSION_base(4,9,0)
-- | @since 1.0.4
instance GEq ((:~~:) a) where
    geq (HRefl :: a :~~: b) (HRefl :: a :~~: c) = Just (Refl :: b :~: c)
#endif

instance (GEq a, GEq b) => GEq (Sum a b) where
    geq (InL x) (InL y) = geq x y
    geq (InR x) (InR y) = geq x y
    geq _ _ = Nothing

instance (GEq a, GEq b) => GEq (Product a b) where
    geq (Pair x y) (Pair x' y') = do
        Refl <- geq x x'
        Refl <- geq y y'
        return Refl

#if MIN_VERSION_base(4,6,0)
-- | @since 1.0.4
instance (GEq f, GEq g) => GEq (f :+: g) where
  geq (L1 x) (L1 y) = geq x y
  geq (R1 x) (R1 y) = geq x y
  geq _ _ = Nothing

-- | @since 1.0.4
instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (x :*: y) (x' :*: y') = do
        Refl <- geq x x'
        Refl <- geq y y'
        return Refl
#endif

#if MIN_VERSION_base(4,10,0)
instance GEq TR.TypeRep where
    geq = testEquality
#endif

-------------------------------------------------------------------------------
-- GCompare
-------------------------------------------------------------------------------

-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where
-- they are equal their parameter types can be unified.
#if __GLASGOW_HASKELL__ >= 810
type GOrdering :: k -> k -> Type
#endif
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b
#if __GLASGOW_HASKELL__ >=708
  deriving Typeable
#endif

-- |TODO: Think of a better name
--
-- This operation forgets the phantom types of a 'GOrdering' value.
weakenOrdering :: GOrdering a b -> Ordering
weakenOrdering GLT = LT
weakenOrdering GEQ = EQ
weakenOrdering GGT = GT

instance Eq (GOrdering a b) where
    x == y = weakenOrdering x == weakenOrdering y

instance Ord (GOrdering a b) where
    compare x y = compare (weakenOrdering x) (weakenOrdering y)

instance Show (GOrdering a b) where
    showsPrec _ GGT = showString "GGT"
    showsPrec _ GEQ = showString "GEQ"
    showsPrec _ GLT = showString "GLT"

instance GShow (GOrdering a) where
    gshowsPrec = showsPrec

instance GRead (GOrdering a) where
    greadsPrec _ s = case con of
        "GGT"   -> [(mkSome GGT, rest)]
        "GEQ"   -> [(mkSome GEQ, rest)]
        "GLT"   -> [(mkSome GLT, rest)]
        _       -> []
        where (con, rest) = splitAt 3 s

-- |Type class for comparable GADT-like structures.  When 2 things are equal,
-- must return a witness that their parameter types are equal as well ('GEQ').
#if __GLASGOW_HASKELL__ >= 810
type GCompare :: (k -> Type) -> Constraint
#endif
class GEq f => GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

instance GCompare ((:~:) a) where
    gcompare Refl Refl = GEQ

#if MIN_VERSION_base(4,9,0)
-- | @since 1.0.4
instance GCompare ((:~~:) a) where
    gcompare HRefl HRefl = GEQ
#endif

#if MIN_VERSION_base(4,10,0)
instance GCompare TR.TypeRep where
    gcompare t1 t2 =
      case testEquality t1 t2 of
        Just Refl -> GEQ
        Nothing ->
          case compare (TR.SomeTypeRep t1) (TR.SomeTypeRep t2) of
            LT -> GLT
            GT -> GGT
            EQ -> error "impossible: 'testEquality' and 'compare' \
                        \are inconsistent for TypeRep; report this \
                        \as a GHC bug"
#endif

defaultCompare :: GCompare f => f a -> f b -> Ordering
defaultCompare x y = weakenOrdering (gcompare x y)

instance (GCompare a, GCompare b) => GCompare (Sum a b) where
    gcompare (InL x) (InL y) = gcompare x y
    gcompare (InL _) (InR _) = GLT
    gcompare (InR _) (InL _) = GGT
    gcompare (InR x) (InR y) = gcompare x y

instance (GCompare a, GCompare b) => GCompare (Product a b) where
    gcompare (Pair x y) (Pair x' y') = case gcompare x x' of
        GLT -> GLT
        GGT -> GGT
        GEQ -> case gcompare y y' of
            GLT -> GLT
            GEQ -> GEQ
            GGT -> GGT

#if MIN_VERSION_base(4,6,0)
-- | @since 1.0.4
instance (GCompare f, GCompare g) => GCompare (f :+: g) where
    gcompare (L1 x) (L1 y) = gcompare x y
    gcompare (L1 _) (R1 _) = GLT
    gcompare (R1 _) (L1 _) = GGT
    gcompare (R1 x) (R1 y) = gcompare x y

-- | @since 1.0.4
instance (GCompare a, GCompare b) => GCompare (a :*: b) where
    gcompare (x :*: y) (x' :*: y') = case gcompare x x' of
        GLT -> GLT
        GGT -> GGT
        GEQ -> case gcompare y y' of
            GLT -> GLT
            GEQ -> GEQ
            GGT -> GGT
#endif

-------------------------------------------------------------------------------
-- Some
-------------------------------------------------------------------------------

-- | Existential. This is type is useful to hide GADTs' parameters.
--
-- >>> data Tag :: * -> * where TagInt :: Tag Int; TagBool :: Tag Bool
-- >>> instance GShow Tag where gshowsPrec _ TagInt = showString "TagInt"; gshowsPrec _ TagBool = showString "TagBool"
-- >>> classify s = case s of "TagInt" -> [mkGReadResult TagInt]; "TagBool" -> [mkGReadResult TagBool]; _ -> []
-- >>> instance GRead Tag where greadsPrec _ s = [ (r, rest) | (con, rest) <-  lex s, r <- classify con ]
--
-- With Church-encoding youcan only use a functions:
--
-- >>> let y = mkSome TagBool
-- >>> y
-- mkSome TagBool
--
-- >>> withSome y $ \y' -> case y' of { TagInt -> "I"; TagBool -> "B" } :: String
-- "B"
--
-- or explicitly work with 'S'
--
-- >>> let x = S $ \f -> f TagInt
-- >>> x
-- mkSome TagInt
--
-- >>> case x of S f -> f $ \x' -> case x' of { TagInt -> "I"; TagBool -> "B" } :: String
-- "I"
--
-- The implementation of 'mapSome' is /safe/.
--
-- >>> let f :: Tag a -> Tag a; f TagInt = TagInt; f TagBool = TagBool
-- >>> mapSome f y
-- mkSome TagBool
--
-- but you can also use:
--
-- >>> withSome y (mkSome . f)
-- mkSome TagBool
--
-- >>> read "Some TagBool" :: Some Tag
-- mkSome TagBool
--
-- >>> read "mkSome TagInt" :: Some Tag
-- mkSome TagInt
--
#if __GLASGOW_HASKELL__ >= 810
type Some :: (k -> Type) -> Type
#endif
newtype Some tag = S
    { -- | Eliminator.
      withSome :: forall r. (forall a. tag a -> r) -> r
    }

#if __GLASGOW_HASKELL__ >= 708
type role Some representational
#endif

-- | Constructor.
mkSome :: tag a -> Some tag
mkSome t = S (\f -> f t)

-- | Map over argument.
mapSome :: (forall x. f x -> g x) ->  Some f -> Some g
mapSome nt (S fx) = S (\f -> fx (f . nt))

-- | @'flip' 'withSome'@
foldSome :: (forall a. tag a -> b) -> Some tag -> b
foldSome some (S thing) = thing some

-- | Traverse over argument.
traverseSome :: Functor m => (forall a. f a -> m (g a)) -> Some f -> m (Some g)
traverseSome f x = withSome x $ \x' -> fmap mkSome (f x')

-- | Monadic 'withSome'.
--
-- @since 1.0.1
withSomeM :: Monad m => m (Some tag) -> (forall a. tag a -> m r) -> m r
withSomeM m k = m >>= \s -> withSome s k

-------------------------------------------------------------------------------
-- Church Some instances
-------------------------------------------------------------------------------

instance GShow tag => Show (Some tag) where
    showsPrec p some = withSome some $ \thing -> showParen (p > 10)
        ( showString "mkSome "
        . gshowsPrec 11 thing
        )

instance GRead f => Read (Some f) where
    readsPrec p = readParen (p>10) $ \s ->
        [ (withSome withTag mkSome, rest')
        | (con, rest) <- lex s
        , con == "Some" || con == "mkSome"
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

instance Control.Applicative.Applicative m => Data.Semigroup.Semigroup (Some m) where
    m <> n =
        withSome m $ \m' ->
        withSome n $ \n' ->
        mkSome (m' *> n')

instance Applicative m => Data.Monoid.Monoid (Some m) where
    mempty = mkSome (pure ())
    mappend = (<>)
