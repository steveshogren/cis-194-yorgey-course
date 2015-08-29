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

doer = do
  x <- getFile "input.dt"
  return $ map matchWith $ makeDigitTable $ breakIntoThrees x
