{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds           #-}
#endif
#if __GLASGOW_HASKELL__ >= 704
#define GHC __GLASGOW_HASKELL__
#if (GHC >= 704 && GHC <707) || GHC >= 801
{-# LANGUAGE Safe                #-}
#else
{-# LANGUAGE Trustworthy         #-}
#endif
#undef GHC
#endif
module Data.GADT.Internal where

import Control.Applicative  (Applicative (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Maybe           (isJust, isNothing)
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.Type.Equality   ((:~:) (..))

#if __GLASGOW_HASKELL__ >=708
import Data.Typeable (Typeable)
#endif

#if MIN_VERSION_base(4,10,0)
import           Data.Type.Equality (testEquality)
import qualified Type.Reflection    as TR
#endif

-- $setup
-- >>> :set -XKindSignatures -XGADTs

-- |'Show'-like class for 1-type-parameter GADTs.  @GShow t => ...@ is equivalent to something
-- like @(forall a. Show (t a)) => ...@.  The easiest way to create instances would probably be
-- to write (or derive) an @instance Show (T a)@, and then simply say:
--
-- > instance GShow t where gshowsPrec = showsPrec
class GShow t where
    gshowsPrec :: Int -> t a -> ShowS

gshows :: GShow t => t a -> ShowS
gshows = gshowsPrec (-1)

gshow :: (GShow t) => t a -> String
gshow x = gshows x ""

instance GShow ((:~:) a) where
    gshowsPrec _ Refl = showString "Refl"

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

-- |@GReadS t@ is equivalent to @ReadS (forall b. (forall a. t a -> b) -> b)@, which is
-- in turn equivalent to @ReadS (Exists t)@ (with @data Exists t where Exists :: t a -> Exists t@)
type GReadS t = String -> [(Some t, String)]

getGReadResult :: Some tag -> (forall a. tag a -> b) -> b
getGReadResult t k = withSome t k

mkGReadResult :: tag a -> Some tag
mkGReadResult = mkSome

-- |'Read'-like class for 1-type-parameter GADTs.  Unlike 'GShow', this one cannot be
-- mechanically derived from a 'Read' instance because 'greadsPrec' must choose the phantom
-- type based on the 'String' being parsed.
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
-- >>> greadMaybe "garbage" mkSome :: Maybe (Some ((:~:) Int))
-- Nothing
--
greadMaybe :: GRead t => String -> (forall a. t a -> b) -> Maybe b
greadMaybe s g = case [f | (f, "") <- greads s] of
    (x : _) -> Just (withSome x g)
    _       -> Nothing

instance GRead ((:~:) a) where
    greadsPrec p s = readsPrec p s >>= f
      where
        f :: forall x. (x :~: x, String) -> [(Some ((:~:) x), String)]
        f (Refl, rest) = return (mkSome Refl, rest)

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

-------------------------------------------------------------------------------
-- GEq
-------------------------------------------------------------------------------

-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the equality of types
-- occurring within them.
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

instance (GEq a, GEq b) => GEq (Sum a b) where
    geq (InL x) (InL y) = geq x y
    geq (InR x) (InR y) = geq x y
    geq _ _ = Nothing

instance (GEq a, GEq b) => GEq (Product a b) where
    geq (Pair x y) (Pair x' y') = do
        Refl <- geq x x'
        Refl <- geq y y'
        return Refl

#if MIN_VERSION_base(4,10,0)
instance GEq TR.TypeRep where
    geq = testEquality
#endif

-------------------------------------------------------------------------------
-- GCompare
-------------------------------------------------------------------------------

-- This instance seems nice, but it's simply not right:
--
-- > instance GEq StableName where
-- >     geq sn1 sn2
-- >         | sn1 == unsafeCoerce sn2
-- >             = Just (unsafeCoerce Refl)
-- >         | otherwise     = Nothing
--
-- Proof:
--
-- > x <- makeStableName id :: IO (StableName (Int -> Int))
-- > y <- makeStableName id :: IO (StableName ((Int -> Int) -> Int -> Int))
-- >
-- > let Just boom = geq x y
-- > let coerce :: (a :~: b) -> a -> b; coerce Refl = id
-- >
-- > coerce boom (const 0) id 0
-- > let "Illegal Instruction" = "QED."
--
-- The core of the problem is that 'makeStableName' only knows the closure
-- it is passed to, not any type information.  Together with the fact that
-- the same closure has the same StableName each time 'makeStableName' is
-- called on it, there is serious potential for abuse when a closure can
-- be given many incompatible types.


-- |A type for the result of comparing GADT constructors; the type parameters
-- of the GADT values being compared are included so that in the case where
-- they are equal their parameter types can be unified.
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
class GEq f => GCompare f where
    gcompare :: f a -> f b -> GOrdering a b

instance GCompare ((:~:) a) where
    gcompare Refl Refl = GEQ

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
newtype Some tag = S
    { -- | Eliminator.
      withSome :: forall r. (forall a. tag a -> r) -> r
    }

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
