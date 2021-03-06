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

-- this prevents the default "show" from looping infinitely
instance (Show a) => Show(Stream a) where 
 show (Cons a b) = show a ++ "," ++ (concat . map (\s -> show s ++ ",") . take 20 . streamToList) b

streamToList :: Stream a -> [a]
streamToList (Cons f r) = f:(streamToList r)

-- streamRepeat 3 == 3333333333
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

-- streamMap (+ 1) $ streamRepeat 2 == 3333333333
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) $ streamMap f b

-- streamFromSeed (+1) 0 == 1234567891011
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s =
  let seed = f s
  in Cons seed $ streamFromSeed f seed
        
nats :: Stream Integer
nats = streamFromSeed (+1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 b1) (Cons a2 b2) = Cons a1 $ Cons a2 $ interleaveStreams b1 b2

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0)
                          (interleaveStreams (streamRepeat 1) (streamFromSeed (+1) 1))


