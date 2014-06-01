toDigits :: Integer -> [Integer]
toDigits t = map (\s -> (read [s]) :: Integer) $ show t

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits
