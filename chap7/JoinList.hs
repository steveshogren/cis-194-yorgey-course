import Data.Monoid
import Sized

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

--        4
--    2        2
-- 1a    1a  1a  1a
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ (Single _ v) = Just v
indexJ i (Append cnt l r) =
  let ind = i + 1
      dub = ind*2
      count = getSize(size(cnt))
  in if ind > count
     then Nothing
     else if count > dub
          then indexJ i l
          else indexJ (i-(dub-count)) r 

