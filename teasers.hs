{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List(groupBy, sortBy)
import Data.Function(on)

combine :: [a] -> [a] -> [a]
combine [] [] = []
combine [] (l2h:l2s) = l2h : l2s
combine (l1h:l1s) [] = l1h : l1s
combine (l1h:l1s) (l2h:l2s) = [l1h,l2h] ++ combine l1s l2s


iFib n prev next acc =
  if n == length acc
  then acc
  else iFib n next (prev+next) (next:acc)

nthFib n = reverse $ iFib n 0 1 [0]

firstDigit = head . show
firstDigitOfList = firstDigit . head

sortLargestTotal n =
  reverse $
  sortBy (compare `on` firstDigitOfList) $
         groupBy ((==) `on` firstDigit) n

main :: IO ()
main = putStrLn "test"
