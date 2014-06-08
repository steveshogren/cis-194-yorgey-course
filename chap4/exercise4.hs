import Data.List
--fun1 :: [Integer] -> Integer
--fun1 [] = 1
--fun1 (x:xs)
--  | even x = (x - 2) * fun1 xs
--  | otherwise = fun1 xs


fun1 :: [Integer] -> Integer
fun1 = foldr (\s accum->(s-2)*accum) 1 . filter even 


--fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n | even n = n + fun2 (n ‘div‘ 2)
--       | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr adder Leaf . concat . (\(a,b) -> transpose [a,b]) . splitHalf . sort

adder :: (Ord a) => a -> Tree a -> Tree a 
adder a Leaf = Node 0 Leaf a Leaf 
adder a t@(Node hi left num right) = 
    if a > num then 
        let nl = adder a left 
        in Node (1 + hi) nl num right
    else 
        let nr = adder a right 
        in Node (1 + hi) left num nr
  
splitHalf :: [a] -> ([a],[a]) 
splitHalf l = splitAt ((length l + 1) `div` 2) l

-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False
xor :: [Bool] -> Bool
xor = foldr (\s acum -> if s then not acum else acum) False     

-- mapP (+1) [1,2,3,4] == [2,3,4,5]
mapP :: (a -> b) -> [a] -> [b]
mapP f = foldr (\s acum -> f s : acum) []
