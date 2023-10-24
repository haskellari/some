# 1.0.6

- Add instances for `SSymbol`, `SNat` and `SChar` from `base >=4.18.0.0'

# 1.0.5

- Add EqP and OrdP classes.
  These are strong versions of Eq1 and Ord1, and on the other hand
  weaker versions of `GEq` and `GCompare`.
  They are exactly what's needed for `Eq` and `Ord` instances of `Some`.

  The naming is unfortunate: `GShow` would be better named `ShowP`,
  as it's similar version of `Show1`.

  Note: we could add `ReadP` with `readsPrecP :: Int -> ReadS (t a)` method,
  but it will barely have any instances.
  `GRead` is different, as it can reify the type index for many types,
  e.g. for the singletons.

  In some future there will be major version of `some` with following
  breaking changes:
  - `EqP` and `OrdP` will become superclasses of `GEq` and `GCompare`
  - `Eq (Some t)` will require `EqP t`, similarly for `Ord` and `OrdP`.
  - `GShow` will get `forall a. Show (f a)` superclass. (This will cause removal of `Product` and `Sum` instances for `base <4.18`).

  To ease future transition you may
  - Define `EqP` and `OrdP` instances for your types.
    The `defaultEq` and `defaultCompare` methods can be used to define
    `eqp` and `comparep` from `GEq` and `GCompare` instances respectively.
  - Move to use `GHC.Generics.:*:` and `:+:` instead of `Data.Functor.Product` and `Sum`, as these have better `Eq` and `Ord` instances.

# 1.0.4.1

- Drop support for GHC before 8.6

# 1.0.4

- Add instances for `(:~~:)`
- Add instances for `:+:` and `:*:`
- Add `defaultGeq :: GCompare f => f a -> f b -> Maybe (a :~: b)`
- Add `defaultGshowsPrec :: Show (t a) => Int -> t a -> ShowS`

# 1.0.3

- Make `GNFData` PolyKinded.
- Add `GNFData ((:~:) a)` and `GNFData TypeRep` instances

# 1.0.2

- Explicitly mark `Data.Some` as `Safe`.
  It was previously inferred, yet it was Safe too,
  as it only re-exports other explicitly marked modules.
- Allow `base-4.15`, GHC-9.0 compatibility

# 1.0.1

- Add 'withSomeM' combinator.
  Allows to workaround: https://gitlab.haskell.org/ghc/ghc/issues/15681

# 1.0.0.3

- One less `unsafeCoerce` (thanks to David Feuer)

# 1.0.0.2

- Broken release

# 1.0.0.1

- Fix issue with GHC#9585 https://gitlab.haskell.org/ghc/ghc/issues/9584

# 1

- Split out of `dependent-sum`
- Have `GADT`, `Newtype`, `Church` variants
- Add `NFData` instance
