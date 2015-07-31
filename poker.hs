{-# LANGUAGE TupleSections #-}

import Data.Time
import Control.Monad
import Control.Applicative

day (_, _, d) = d

addToDay :: UTCTime -> Integer -> Int
addToDay today days =
  day . toGregorian . addDays days . utctDay $ today

buildDate goal today daysFuture =
  let dayNumber = addToDay today daysFuture
  in ((goal * daysFuture) + 2800, dayNumber)

dailyCounts goal today =
 fmap (buildDate goal today) [1..30]

main :: IO ()
main = do
  today <- getCurrentTime
  print $ dailyCounts 333 today
