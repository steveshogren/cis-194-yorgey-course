{-# LANGUAGE TupleSections #-}

import Data.Time
import Control.Monad
import Control.Applicative

day (_, _, d) = d

todayPlus :: Integer -> IO Int
todayPlus days =
  liftM (day . toGregorian . addDays days . utctDay) getCurrentTime

buildDate :: Integer -> Integer -> IO (Integer, Int)
buildDate goal daysFuture =
  ((goal * daysFuture) + 2800, ) <$> todayPlus daysFuture

dailyCounts goal =
  sequence $ fmap (buildDate goal) [1..30]

main :: IO ()
main = do
  t <- dailyCounts 333
  print t
