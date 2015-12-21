{-# LANGUAGE FlexibleInstances  #-}

import MonoidAverage as A
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Monoid

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Tests" [
        testGroup "Tests Rational" (propertiesFor (0 :: Rational))
      , testGroup "Tests Double" (propertiesFor (0 :: Double))
  ]

propertiesFor :: (Show a, Arbitrary a, Fractional a, TestEq a) =>
                 a -> [TestTree]
propertiesFor t = [
          testProperty "Average result" $ propAverageResult t
        , testProperty "Monoid identity law" $ propIdentityLaw t
        , testProperty "Monoid associative law" $ propAssocLaw t
   ]

class TestEq a where
    testEq :: a -> a -> Bool

instance TestEq Double where
    testEq x y = abs (x - y) < 0.00001

instance TestEq Rational where
    testEq = (==)

averageFromList :: Fractional a => [a] -> Average a
averageFromList = mconcat . map A.average

averageEq :: (Fractional a, TestEq a) => Average a -> Average a -> Bool
averageEq x y = getAverage x `testEq` getAverage y

propAverageResult :: (Fractional a, TestEq a) => a -> [a] -> Property
propAverageResult _ xs = not (null xs) ==>
                         averageRef xs `testEq` averageMonoid xs
    where averageRef xs' = sum xs' / fromIntegral (length xs')
          averageMonoid = A.getAverage . averageFromList

propIdentityLaw :: (Fractional a, TestEq a) => a -> [a] -> Property
propIdentityLaw _ xs = not (null xs) ==>
                     (avg <> mempty) `averageEq` avg
                     && (mempty <> avg) `averageEq` avg
    where avg = averageFromList xs

propAssocLaw ::  (Fractional a, TestEq a) => a -> [a] -> [a] -> [a] -> Property
propAssocLaw _ xs ys zs = not (null xs) || not (null ys) || not (null zs) ==>
                          ((avg_xs <> avg_ys) <> avg_zs)
                          `averageEq`
                          (avg_xs <> (avg_ys <> avg_zs))
    where avg_xs = averageFromList xs
          avg_ys = averageFromList ys
          avg_zs = averageFromList zs
