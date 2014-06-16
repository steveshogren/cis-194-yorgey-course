import Data.Monoid
import Sized
import Debug.Trace

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
     deriving (Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m 
tag (Append m _ _) = m 

a = Single (Size 1) "a"
b = Single (Size 1) "b"
c = Single (Size 1) "c"
d = Single (Size 1) "d"

-- indexJ 0 $ (a +++ b) +++ (c +++ d) == Just "a"
-- indexJ 1 $ (a +++ b) +++ (c +++ d) == Just "b"
-- indexJ 2 $ (a +++ b) +++ (c +++ d) == Just "c"
-- indexJ 3 $ (a +++ b) +++ (c +++ d) == Just "d"
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ (Single _ v) = Just v
indexJ i (Append cnt l r) =
  let dub = i*2
      count = getSize(size(cnt))
  in if i > (count-1)
     then Nothing
     else if count > dub
          then indexJ i l
          else indexJ (if dub == count then (dub-count) else (i-(dub-count))) r 

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

-- (indexJ 0 lister) == (jlToList lister !!? 0) == True
-- (indexJ 3 lister) == (jlToList lister !!? 3) == True
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

--           6
--      3         3
--   1     2    1   2
--        1 1      1 1
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl 
dropJ i (Single _ _) | i > 0  = Empty
dropJ i n@(Append cnt l r) =
  let dub = i*2
      count = getSize(size(cnt))
      inLower = count > dub
      dropWholeList = i >= count
      nextIndex = i-(dub-(count*2)) 
      out = " | i:" ++ show i ++ " count: " ++ show count ++ " dub: " ++ show dub ++ " nextI: " ++ show nextIndex
  in case () of
    _ | dropWholeList -> trace ("Drop Whole" ++ out) Empty
      | inLower       -> trace ("Drop to left" ++ out) $ dropJ i l
      | otherwise     -> trace ("Drop to right" ++ out) $ dropJ nextIndex r

