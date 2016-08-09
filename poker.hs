import Data.Time
import Data.Time.Format
import Control.Monad
import Control.Applicative

day :: (t, t1, t2) -> t2
day (_, _, d) = d

addToDay :: UTCTime -> Integer -> Day
addToDay today days =
  addDays days . utctDay $ today

printDay d = formatTime defaultTimeLocale "   %a - %b %e %Y" d

buildDate goal today daysFuture =
  let dayNumber = addToDay today daysFuture
  in (show ((goal * daysFuture) + 2012)) ++ printDay dayNumber

dailyCounts goal today =
 fmap (buildDate goal today) [1..35]

-- patrick mentioned clojure juxt does the same signiture as haskell fmap ($ a)
-- which is like [a -> b] -> a -> [b]
testDollar :: [Integer]
testDollar = fmap ($ 4) [(1 +), (3 -), (4 *)] 

fizzer :: (Integral a, Show a) => a -> String
fizzer number =
  if rem number 3 /= 0 && rem number 5 /= 0
  then show number
  else
    let f = if rem number 3 == 0 then "fizz" else ""
        b = if rem number 5 == 0 then "buzz" else ""
        in f ++ b

fizzbuzz :: [String]
fizzbuzz = map fizzer [1..100]

main :: IO [()]
main = do
  today <- getCurrentTime
  sequence $ map (putStrLn . show) $ dailyCounts 333 today

add :: Num a => a -> a -> a
add x y = x + y

divide :: Fractional a => a -> a -> a
divide x y = x / y

concat2 :: [a] -> [a] -> [a]
concat2 str1 str2 = str1 ++ str2
-- SPC m s b  -  compiles file

(|>) a b = b a

helloWorld2 :: [a] -> [a] -> [a] -> [a]
helloWorld2 x y z =
  z ++ x ++ y

helloWorld x =
  let name = "hello: " ++ x
  in name |> print
