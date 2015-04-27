import System.FilePath
import System.Directory
import Data.String.Utils
import Data.List.Utils
import Data.List (group, sort)
import Control.Monad
import System.FilePath.Find
import Filesystem.Path.CurrentOS
import Filesystem.Path
import System.Process
import Data.List.Split (splitOn)
import System.Time.Utils
import System.Time
import System.Locale
import Data.Function (on)

toInt :: String -> Int
toInt = read

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const .drop 1) xs (drop n xs)

getCommitDates :: String -> IO [ClockTime]
getCommitDates repoPath = do
  hout <- readProcess "git" ["--git-dir", repoPath, "log", "-3", "--pretty=format:\"%at\""] "."
  let x =  splitOn "\n" hout
  return $ map (epochToClockTime . toInt . replace "\"" "") x

main :: IO()
main = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth <=? 3) (fileName ==? ".git") path
  times <- (liftM Prelude.concat . mapM getCommitDates) onlyGits
  let sorted = sort times
  let days = mapM (\x -> do
                     ct <- toCalendarTime x
                     return $ formatCalendarTime defaultTimeLocale "%D" ct)
  grouped <- liftM group $ days sorted
  -- lastTen <- liftM(lastN 10 . fst) grouped
  let uniqueDays = map head grouped
  let lastTen = lastN 10 uniqueDays
  print lastTen
  -- print times
