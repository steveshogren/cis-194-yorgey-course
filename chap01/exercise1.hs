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

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (map toDigits) 

-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
validate :: Integer -> Bool
validate card = 
   let sum = (sumDigits . doubleEveryOther . toDigits) card
   in sum `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
-- hanoi 2 "a" "b" "c" == [("a","c"),("a","b"),("c","b")]
-- hanoi 3 "a" "b" "c" == [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to hold = [(from, to)] 
hanoi n a b c = 
   hanoi (n-1) a c b
     ++ hanoi 1 a b c
     ++ hanoi (n-1) c b a

