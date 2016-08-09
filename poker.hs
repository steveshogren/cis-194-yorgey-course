import Control.Monad
import Control.Applicative


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
