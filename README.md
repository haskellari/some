some
====

This library defines several versions of an existential type 'Some', which works like this datastructure:

    data Some f where
        Some :: f a -> Some f

and operations to work with it.

However, due to [GHC issue #1965](https://gitlab.haskell.org/ghc/ghc/issues/1965), the direct implementation of this datastructure is less efficient than it could be.  As a result, this library uses a more complex approach that implements it as a newtype, so there's no runtime overhead associated with wrapping and unwrapping `Some` values.

To use the recommended implementation, import `Data.Some`.  If you need to ensure that you are definitely using the newtype implementation, import `Data.Some.Newtype`.  By default, these are the same, but this can be changed via a Cabal flag.
