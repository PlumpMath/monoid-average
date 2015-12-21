module Main where

import MonoidAverage
import Data.Monoid

main :: IO ()
main = print $ getAverage $ average (2 :: Double) <> average (4 :: Double)
