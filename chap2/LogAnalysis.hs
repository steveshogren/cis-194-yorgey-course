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

