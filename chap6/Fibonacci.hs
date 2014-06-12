fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : next fibs2
  where
    next (a : t@(b:_)) = (a+b) : next t


data Stream a = Cons a (Stream a)

instance (Show a) => Show(Stream a) where 
 show (Cons a b) = (concat . map show . take 10 . streamToList) b

streamToList :: Stream a -> [a]
streamToList (Cons f r) = f:(streamToList r)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a


