module MonoidAverage
    (
      Average
    , average
    , getAverage
    ) where

import qualified Data.Monoid as M

data Average a = Average !Int !a

instance Num a => M.Monoid (Average a) where
    mempty = Average 0 0
    Average n a `mappend` Average m b = Average (n + m) (a + b)

average :: Fractional a => a -> Average a
average = Average 1

getAverage :: Fractional a => Average a -> Maybe a
getAverage (Average n a) | n == 0 = Nothing
                         | otherwise = Just $ a / fromIntegral n
