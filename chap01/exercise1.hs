toDigits :: Integer -> [Integer]
toDigits t  
   | t < 1     = []
   | otherwise = map (\s -> (read [s]) :: Integer) $ show t

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
