{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Data.List.Split

parseMessage :: String -> LogMessage
parseMessage = parseMessageByWord . splitOn " "

readInt :: String -> Int
readInt v = ((read v) :: Int) 

parseMessageByWord :: [String] -> LogMessage
parseMessageByWord ("E":(sev: (time:mes))) = LogMessage (Error (readInt sev)) (readInt time) (concat mes)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
