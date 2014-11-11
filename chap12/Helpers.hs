module Helpers (isSeq, groupEqual) where

import Data.List 

empty = (==0) . length
isSeq = not . empty

groupEqual f = groupBy (\c1 c2 -> (==) (f c1) (f c2)) 
