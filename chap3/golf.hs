-- skips "test" 
-- > ["test","est","st","t"]
skips :: [a] -> [[a]]
skips a = map (\s -> drop s a) [0..(length a)-1] 


--localMaxima [2,9,5,6,1] == [9,6]
localMaxima :: [Integer] -> [Integer]
localMaxima = map (head . drop 1) . filter filterMax . groupPart 3

filterMax :: [Integer] -> Bool
filterMax (a:b:c:_) = b > a && b > c
filterMax _ = False

groupPart :: Int -> [a] -> [[a]]
groupPart c l = map (\s -> (take c . drop s) l) [0..(length l)-c]



