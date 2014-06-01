toDigits :: Integer -> [Integer]
toDigits t  
   | t < 1     = []
   | otherwise = map (\s -> (read [s]) :: Integer) $ show t

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherBackwards . reverse

doubleEveryOtherBackwards :: [Integer] -> [Integer]
doubleEveryOtherBackwards [] = []
doubleEveryOtherBackwards (h:[]) = [h]
doubleEveryOtherBackwards (h:(n:ts)) = h : (* 2) n : doubleEveryOtherBackwards ts
