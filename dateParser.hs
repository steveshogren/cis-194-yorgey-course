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
import Data.Time.Clock ()
import Data.Time.Calendar ()
import Data.Time.LocalTime ()
import System.Environment
import System.Exit
import Data.Time
import Control.Applicative ((<$>))

toInt :: String -> Int
toInt = read

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const .drop 1) xs (drop n xs)

getCommitDates :: String -> IO [ClockTime]
getCommitDates repoPath = do
  hout <- readProcess "git" ["--git-dir", repoPath, "log", "--pretty=format:\"%at\""] "."
  let x =  splitOn "\n" hout
  return $ map (epochToClockTime . toInt . replace "\"" "") x

stringNDaysAgo :: Integer -> IO String
stringNDaysAgo n = do
  time <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
  return $ showGregorian $ addDays (-n) $ localDay time

generateLastNDays :: Integer -> IO [String]
generateLastNDays n = liftM reverse $ mapM stringNDaysAgo [0..(n-1)]

convertClockToString :: ClockTime -> IO String
convertClockToString clk = do
  ct <- toCalendarTime clk
  return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d" ct

firstMissing :: Eq a => [a] -> [a] -> a
firstMissing (expected:exs) (actual:acs) =
  if expected /= actual then expected else firstMissing exs acs

getTime :: IO String
getTime =
  formatTime defaultTimeLocale "%H:%M:%S"
  <$> liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime

makeDateString :: String -> String -> String
makeDateString t d =
  "    --date=\"" ++ d ++ " " ++ t ++ "\""

getOldestMissing :: IO ()
getOldestMissing = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth <=? 3) (fileName ==? ".git") path
  times <- mapM getCommitDates $ onlyGits ++ ["/home/jack/.emacs.bak/.git", "/home/jack/.emacs.d/private/.git"]
  grouped <- liftM group $ mapM convertClockToString . sort . concat $ times
  let uniqueDays = map head grouped
  let actualDays = lastN 40 uniqueDays
  expectedDays <- generateLastNDays 40

  if expectedDays == actualDays then print ()
  else do
    time <- getTime
    putStrLn $ makeDateString time $ firstMissing  expectedDays  actualDays

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["-c"] = getOldestMissing
parse [] = getOldestMissing

usage :: IO ()
usage   = putStrLn "Usage: gitCleaner [-vhcn] "
version :: IO ()
version = putStrLn "Haskell gitCleaner 0.1"
exit :: IO a
exit    = exitWith ExitSuccess
die :: IO a
die     = exitWith (ExitFailure 1)


  -- let actualDays = ["2015-04-08","2015-04-09","2015-04-10","2015-04-11" ,"2015-04-12","2015-04-13","2015-04-14","2015-04-15","2015-04-16","2015-04-17","2015-04-18","2015-04-19","2015-04-20","2015-04-21","2015-04-22","2015-04-23","2015-04-24","2015-04-25","2015-04-26","2015-04-27"]
  -- let expectedDays = ["2015-04-08","2015-04-09","2015-04-10","2015-04-11","2015-04-12","2015-04-13","2015-04-14","2015-04-15","2015-04-16","2015-04-17","2015-04-18","2015-04-19","2015-04-20","2015-04-21","2015-04-22","2015-04-23","2015-04-24","2015-04-25","2015-04-26","2015-04-27"]
