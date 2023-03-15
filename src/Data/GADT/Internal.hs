{-# LANGUAGE CPP                      #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RoleAnnotations          #-}
{-# LANGUAGE Safe                     #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
module Data.GADT.Internal where

import Control.Applicative  (Applicative (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Kind            (Type)
import Data.Maybe           (isJust, isNothing)
import Data.Monoid          (Monoid (..))
import Data.Semigroup       (Semigroup (..))
import Data.Type.Equality   (TestEquality (..), (:~:) (..), (:~~:) (..))
import GHC.Generics         ((:*:) (..), (:+:) (..))

import qualified Type.Reflection as TR

#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Constraint)
#endif

#if MIN_VERSION_base(4,18,0)
import qualified GHC.TypeLits as TL
import qualified GHC.TypeNats as TN
#endif

-- $setup
-- >>> :set -XKindSignatures -XGADTs -XTypeOperators -XStandaloneDeriving -XQuantifiedConstraints
-- >>> import Data.Type.Equality
-- >>> import Data.Functor.Sum
-- >>> import Data.Maybe (isJust, isNothing)
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

-- | @since 1.0.4
instance GShow ((:~~:) a) where
    gshowsPrec _ HRefl = showString "HRefl"

instance GShow TR.TypeRep where
    gshowsPrec = showsPrec

#if MIN_VERSION_base(4,18,0)
instance GShow TL.SChar where
    gshowsPrec = showsPrec

instance GShow TL.SSymbol where
    gshowsPrec = showsPrec

instance GShow TN.SNat where
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

-- | @since 1.0.4
instance k1 ~ k2 => GRead ((:~~:) (a :: k1) :: k2 -> Type) where
    greadsPrec _ = readParen False (\s ->
        [ (S $ \k -> k (HRefl :: a :~~: a), t)
        | ("HRefl", t) <- lex s
        ])

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

-------------------------------------------------------------------------------
-- GEq
-------------------------------------------------------------------------------

-- |A class for type-contexts which contain enough information
-- to (at least in some cases) decide the equality of types
-- occurring within them.
--
-- This class is sometimes confused with 'TestEquality' from base.
-- 'TestEquality' only checks /type equality/.
--
-- Consider
--
-- >>> data Tag a where TagInt1 :: Tag Int; TagInt2 :: Tag Int
--
-- The correct @'TestEquality' Tag@ instance is
--
-- >>> :{
-- instance TestEquality Tag where
--     testEquality TagInt1 TagInt1 = Just Refl
--     testEquality TagInt1 TagInt2 = Just Refl
--     testEquality TagInt2 TagInt1 = Just Refl
--     testEquality TagInt2 TagInt2 = Just Refl
-- :}
--
-- While we can define
--
-- @
-- instance 'GEq' Tag where
--    'geq' = 'testEquality'
-- @
--
-- this will mean we probably want to have
--
-- @
-- instance 'Eq' Tag where
--    _ '==' _ = True
-- @
--
-- /Note:/ In the future version of @some@ package (to be released around GHC-9.6 / 9.8) the
-- @forall a. Eq (f a)@ constraint will be added as a constraint to 'GEq',
-- with a law relating 'GEq' and 'Eq':
--
-- @
-- 'geq' x y = Just Refl   ⇒  x == y = True        ∀ (x :: f a) (y :: f b)
-- x == y                ≡  isJust ('geq' x y)     ∀ (x, y :: f a)
-- @
--
-- So, the more useful @'GEq' Tag@ instance would differentiate between
-- different constructors:
--
-- >>> :{
-- instance GEq Tag where
--     geq TagInt1 TagInt1 = Just Refl
--     geq TagInt1 TagInt2 = Nothing
--     geq TagInt2 TagInt1 = Nothing
--     geq TagInt2 TagInt2 = Just Refl
-- :}
--
-- which is consistent with a derived 'Eq' instance for 'Tag'
--
-- >>> deriving instance Eq (Tag a)
--
-- Note that even if @a ~ b@, the @'geq' (x :: f a) (y :: f b)@ may
-- be 'Nothing' (when value terms are inequal).
--
-- The consistency of 'GEq' and 'Eq' is easy to check by exhaustion:
--
-- >>> let checkFwdGEq :: (forall a. Eq (f a), GEq f) => f a -> f b -> Bool; checkFwdGEq x y = case geq x y of Just Refl -> x == y; Nothing -> True
-- >>> (checkFwdGEq TagInt1 TagInt1, checkFwdGEq TagInt1 TagInt2, checkFwdGEq TagInt2 TagInt1, checkFwdGEq TagInt2 TagInt2)
-- (True,True,True,True)
--
-- >>> let checkBwdGEq :: (Eq (f a), GEq f) => f a -> f a -> Bool; checkBwdGEq x y = if x == y then isJust (geq x y) else isNothing (geq x y)
-- >>> (checkBwdGEq TagInt1 TagInt1, checkBwdGEq TagInt1 TagInt2, checkBwdGEq TagInt2 TagInt1, checkBwdGEq TagInt2 TagInt2)
-- (True,True,True,True)
--
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

-- | @since 1.0.4
instance GEq ((:~~:) a) where
    geq (HRefl :: a :~~: b) (HRefl :: a :~~: c) = Just (Refl :: b :~: c)

instance (GEq a, GEq b) => GEq (Sum a b) where
    geq (InL x) (InL y) = geq x y
    geq (InR x) (InR y) = geq x y
    geq _ _ = Nothing

instance (GEq a, GEq b) => GEq (Product a b) where
    geq (Pair x y) (Pair x' y') = do
        Refl <- geq x x'
        Refl <- geq y y'
        return Refl

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

instance GEq TR.TypeRep where
    geq = testEquality

#if MIN_VERSION_base(4,18,0)
instance GEq TL.SChar where
    geq = testEquality

instance GEq TL.SSymbol where
    geq = testEquality

instance GEq TN.SNat where
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
#if __GLASGOW_HASKELL__ >= 810
type GOrdering :: k -> k -> Type
#endif
data GOrdering a b where
    GLT :: GOrdering a b
    GEQ :: GOrdering t t
    GGT :: GOrdering a b

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

-- | @since 1.0.4
instance GCompare ((:~~:) a) where
    gcompare HRefl HRefl = GEQ

instance GCompare TR.TypeRep where
    gcompare = gcompareSing "TypeRep" TR.SomeTypeRep

#if MIN_VERSION_base(4,18,0)
instance GCompare TL.SChar where
    gcompare = gcompareSing "SChar" TL.fromSChar

instance GCompare TL.SSymbol where
    gcompare = gcompareSing "SSymbol" TL.fromSSymbol

instance GCompare TN.SNat where
    gcompare = gcompareSing "SNat" TN.fromSNat
#endif

defaultCompare :: GCompare f => f a -> f b -> Ordering
defaultCompare x y = weakenOrdering (gcompare x y)

-- | An implementation of 'gcompare' for a singleton type.
gcompareSing :: (TestEquality f, Ord c)
             => String
             -- ^ The name of the singleton type.
             -- (Only used for error message purposes.)
             -> (forall x. f x -> c)
             -- ^ How to turn the singleton type into a value that can be
             -- compared with 'Ord'.
             -> f a -> f b -> GOrdering a b
gcompareSing singName toOrd t1 t2 =
  case testEquality t1 t2 of
    Just Refl -> GEQ
    Nothing ->
      case compare (toOrd t1) (toOrd t2) of
        LT -> GLT
        GT -> GGT
        EQ -> error $ "impossible: 'testEquality' and 'compare' are inconsistent for "
                   ++ singName
                   ++ "; report this as a GHC bug"

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

type role Some representational

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
