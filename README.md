Average Monoid
====

[![Build Status](https://secure.travis-ci.org/dlgd/monoid-average.svg)](http://travis-ci.org/dlgd/monoid-average)


Purpose
====

This package provides a `Monoid` instance to compute the average. It is
particularly useful when dealing with a stream of data with the only objective
to eventually compute the average as it is computed incrementaly with no need to
keep all the data in memory.


Build
====

The recommanded way to build this package is with
[stack](https://github/commercialhaskell/stack) even though it can build
with `cabal` too. Given `stack` is installed on your machine, run:

    stack setup
    stack build
    stack test


Example
====

    ghci> print $ getAverage $ average (2 :: Double) <> average (4 :: Double)
    3.0
    ghci> getAverage $ mconcat $ map average ([3 % 2, 5 % 6] :: [Rational])
    7 % 6
