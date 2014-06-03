{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

parseMessage :: String -> LogMessage
parseMessage = parseMessageByWord . words

readInt :: String -> Int
readInt v = ((read v) :: Int) 

parseMessageByWord :: [String] -> LogMessage
parseMessageByWord ("I":(time:mes)) = LogMessage Info (readInt time) (unwords mes)
parseMessageByWord ("W":(time:mes)) = LogMessage Warning (readInt time) (unwords mes)
parseMessageByWord ("E":(sev: (time:mes))) = LogMessage (Error (readInt sev)) (readInt time) (unwords mes)
parseMessageByWord mes = Unknown (unwords mes)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert lg Leaf = Node Leaf lg Leaf  
insert lg tree =
   let (LogMessage _ time _) = lg  
       (Node rtree (LogMessage _ ttime _) ltree) = tree
       (Node _ ilog _) = tree
   in if time > ttime then Node rtree ilog (insert lg ltree) else Node (insert lg rtree) ilog ltree   

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (h:ts) = (insert h (build ts))

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node rtree lg ltree) =
  inOrder rtree ++ [lg] ++ inOrder ltree

