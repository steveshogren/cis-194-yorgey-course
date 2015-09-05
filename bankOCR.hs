module BankOCR where

import Control.Monad
import Data.List.Split(chunksOf)

getFile :: FilePath -> IO [String]
getFile name = liftM (take 3 . lines) $ readFile name

breakIntoThrees :: [String] -> [[String]]
breakIntoThrees = map (chunksOf 3)

makeDigitTable [[], [], []] = []
makeDigitTable [a, b, c] =
  [[head a, head b, head c]] ++ makeDigitTable [tail a, tail b, tail c]

matchWith :: Num a => [[Char]] -> a
matchWith [" _ ",
           "| |",
           "|_|"] = 0
matchWith ["   ",
           "  |",
           "  |"] = 1
matchWith [" _ ",
           " _|",
           "|_ "] = 2
matchWith [" _ ",
           " _|",
           " _|"] = 3
matchWith ["   ",
           "|_|",
           "  |"] = 4
matchWith [" _ ",
           "|_ ",
           " _|"] = 5
matchWith [" _ ",
           "|_ ",
           "|_|"] = 6
matchWith [" _ ",
           "  |",
           "  |"] = 7
matchWith [" _ ",
           "|_|",
           "|_|"] = 8
matchWith [" _ ",
           "|_|",
           "  |"] = 9

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fib2 =
  let fibi a b n = if (n > 0) then fibi b (a+b) (n-1) else a
  in fibi 0 1

doer = do
  x <- getFile "input.dt"
  return $ map matchWith $ makeDigitTable $ breakIntoThrees x
