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

t = Single (Sum 1) "a"

--        4
--    2        2
-- 1a    1a  1a  1a
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ (Single _ v) = v
indexJ i (Append cnt l r) =
  let mid = ceiling $ size(cnt) / 2
  in if i < mid then indexJ (abs (i-mid)) l else indexJ (abs (i-mid)) r 

