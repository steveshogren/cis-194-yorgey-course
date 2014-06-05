import Data.List
import Data.Function
import Data.Ord

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


histogram :: [Integer] -> String
histogram i =  
   let grp = (groupBy (==) . sort) i
   in (concat . reverse) $
      ["123456789","=========\n"] 
      ++ map (\row -> (map (\s -> if getCount s grp >= row then "*" else "")) [1..9]) [0..getMax grp]

      -- ++ (concat . map (\s -> show $ getCount s grp)) [1..9]
getMax :: [[Integer]] -> Int
getMax = head . head . reverse . sortBy (compare `on` length) 

getCount :: Integer -> [[Integer]] -> Int
getCount n grp =  
    let set = filter (\s -> head s == n)  grp
    in if null set then 0 else (length . head) set

