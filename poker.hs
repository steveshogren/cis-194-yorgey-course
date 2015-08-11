import Data.Time
import Control.Monad
import Control.Applicative

day :: (t, t1, t2) -> t2
day (_, _, d) = d

addToDay :: UTCTime -> Integer -> Int
addToDay today days =
  day . toGregorian . addDays days . utctDay $ today

buildDate :: Integer -> UTCTime -> Integer -> (Integer, Int)
buildDate goal today daysFuture =
  let dayNumber = addToDay today daysFuture
  in ((goal * daysFuture) + 2800, dayNumber)

dailyCounts :: Integer -> UTCTime -> [(Integer, Int)]
dailyCounts goal today =
 fmap (buildDate goal today) [1..30]

fizzer number =
  if rem number 3 == 0 && rem number 5 == 0
  then "fizzbuzz"
  else if rem number 3 == 0
       then "fizz"
       else if rem number 5 == 0
            then "buzz"
            else show number

fizzbuzz = map fizzer [1..100]

main :: IO ()
main = do
  today <- getCurrentTime
  print $ dailyCounts 333 today
