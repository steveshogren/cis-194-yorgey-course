{-# LANGUAGE NoMonomorphismRestriction #-}
import Data.List(groupBy, sortBy, sort)
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

firstDigit :: Show a => a -> Char
firstDigit = head . show

firstDigitOfList :: Show b => [b] -> Char
firstDigitOfList = firstDigit . head

padR :: Char -> Int -> String -> String
padR with n s
    | length s < n  = s ++ replicate (n - length s) with
    | otherwise     = s

widestNum :: (Ord b, Show b) => [b] -> Int
widestNum  =  length . show . maximum

sortGrouping :: (Ord a, Show a) => [a] -> [a]
sortGrouping g =
  let largestNum = widestNum g
      leadChar = head . show . head $ g
  in sortBy (flip compare `on` padR leadChar largestNum . show) g

sortLargestTotal :: (Ord a, Show a) => [a] -> [[a]]
sortLargestTotal =
  map sortGrouping .
  sortBy (flip compare `on` firstDigitOfList) .
  groupBy ((==) `on` firstDigit) .
  sortBy (compare `on` firstDigit)

main :: IO ()
main = print  $ sortLargestTotal [50, 9, 95, 29, 55, 5, 59, 51, 2, 22]
