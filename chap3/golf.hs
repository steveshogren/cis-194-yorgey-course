skips :: [a] -> [[a]]
skips a = map (\s -> drop s a) [0..(length a)-1] 
