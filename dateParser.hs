import System.FilePath
import Data.String.Utils
import Data.List (group, sort)
import Control.Monad
import System.FilePath.Find
import System.Process
import Data.List.Split (splitOn)
import System.Time.Utils
import System.Time
import System.Locale
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (showGregorian, addDays)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, localDay)

toInt :: String -> Int
toInt = read

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const .drop 1) xs (drop n xs)

getCommitDates :: String -> IO [ClockTime]
getCommitDates repoPath = do
  hout <- readProcess "git" ["--git-dir", repoPath, "log", "--pretty=format:\"%at\""] "."
  let x =  splitOn "\n" hout
  return $ map (epochToClockTime . toInt . replace "\"" "") x

stringNDaysAgo n = do
  time <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
  return $ showGregorian $ addDays (-n) $ localDay time

generateLastNDays n = liftM reverse $ mapM stringNDaysAgo [0..n]

main :: IO()
main = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth <=? 3) (fileName ==? ".git") path
  times <- (liftM concat . mapM getCommitDates) $ onlyGits ++ ["/home/jack/.emacs.bak/.git", "/home/jack/.emacs.d/private/.git"]
  let sorted = sort times
  let days = mapM (\x -> do
                     ct <- toCalendarTime x
                     return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d" ct)
  grouped <- liftM group $ days sorted
  let uniqueDays = map head grouped
  let lastTen = lastN 20 uniqueDays
  lastNDays <- generateLastNDays 20
  print $ show $ lastNDays == lastTen
  print lastTen
  print lastNDays
  -- print times
