Average Monoid
====

[![Build Status](https://secure.travis-ci.org/dlgd/monoid-average.svg)](http://travis-ci.org/dlgd/monoid-average)


Purpose
----

This package provides a `Monoid` instance to compute the average. It is
particularly useful when dealing with a stream of data with the only objective
to eventually compute the average. The result can be computed incrementally with
no need to keep all the stream in memory.


Build
----

The preferred way to build this package is by using
[stack](http://www.haskellstack.org). Given `stack` is installed on your
machine, run:

    stack setup
    stack build
    stack test

It is also possible to use cabal by running:

    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests && cabal build && cabal test


API
----

Instanciate an `Average` `monoid` from a value:

```haskell
average :: Fractional a => a -> Average a
```

Retrieve the average value:

```haskell
getAverage :: Fractional a => Average a -> Maybe a
```

Examples
----

```haskell
ghci> getAverage $ average (2 :: Double) <> average (4 :: Double)
Just 3.0
```

```haskell
ghci> getAverage $ (mempty :: Average Double)
Nothing
```

```haskell
ghci> getAverage $ mconcat $ map average ([3 % 2, 5 % 6] :: [Rational])
Just (7 % 6)
```

```haskell
ghci> getAverage $ mconcat $ map average ([] :: [Rational])
Nothing
```
