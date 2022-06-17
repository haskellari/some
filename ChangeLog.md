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
