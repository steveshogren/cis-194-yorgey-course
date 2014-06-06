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


--histogram [1,2,2,4,6,2,2,8,1,1,1]
--"**       \n**       \n**       \n** * * * \n=========\n123456789"
histogram :: [Int] -> String
histogram i =  
   let grp = (groupBy (==) . sort) i
   in (concat . reverse) $ (["123456789","=========\n"] ++ map (makeRow grp) [1..getMax grp])

makeRow :: [[Int]] -> Int -> String
makeRow grp row = (foldl (\accum s -> if getCount s grp >= row 
                                     then accum++"*" 
                                     else accum++" ") "" [1..9]) ++ "\n"

getMax :: [[Int]] -> Int
getMax = length . head . reverse . sortBy (compare `on` length) 

getCount :: Int -> [[Int]] -> Int
getCount n grp =  
    let set = filter (\s -> head s == n)  grp
    in if null set then 0 else (length . head) set

