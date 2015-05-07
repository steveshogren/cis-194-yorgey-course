import System.FilePath
import Data.String.Utils
import Data.List (sort, nub)
import System.FilePath.Find
import System.Process (readProcess)
import Data.List.Split (splitOn)
import System.Time.Utils
import System.Time
import System.Locale
import Data.Time.Clock ()
import Control.Monad(liftM)
import Data.Time.Calendar ()
import Data.Time.LocalTime ()
import System.Environment
import System.Exit
import DateStuff (ExpectedDays, generateLastNDays , getTime, makeDateString)
import qualified Data.Set as Set

type ActualDays = [String]

toInt :: String -> Int
toInt = read

lastN :: Int -> [a] -> [a]
lastN n xs = foldl (const .drop 1) xs (drop n xs)

getGitDirectories :: IO [FilePath]
getGitDirectories = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth <=? 3) (fileName ==? ".git") path
  return $ onlyGits ++ ["/home/jack/.emacs.bak/.git", "/home/jack/.emacs.d/private/.git"]

getCommitDates :: String -> IO [ClockTime]
getCommitDates repoPath = do
  hout <- readProcess "git" ["--git-dir", repoPath, "log", "--pretty=format:\"%at\""] "."
  let x =  splitOn "\n" hout
  return $ map (epochToClockTime . toInt . replace "\"" "") x

updateGitHooks :: IO ()
updateGitHooks = do
  gitDirs <- getGitDirectories
  results <- mapM (\dir -> readProcess "cp" ["/home/jack/.git_template/hooks/post-commit", dir ++ "/hooks/."] ".") gitDirs
  putStrLn . (++ " Success!") . concat $ results

convertClockToString :: ClockTime -> IO String
convertClockToString clk = do
  ct <- toCalendarTime clk
  return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d" ct

firstMissing :: ExpectedDays -> Set.Set String -> String
firstMissing (expected:exs) actual =
  if Set.notMember expected actual then expected else firstMissing exs actual

getLastNGitCommitDays :: Int -> IO ActualDays
getLastNGitCommitDays n = do
  gitDirs <- getGitDirectories
  times <- mapM getCommitDates gitDirs
  liftM (lastN n . nub) $ mapM convertClockToString . sort . concat $ times

getOldestMissing :: IO ()
getOldestMissing = do
  actualDays <- getLastNGitCommitDays 40
  expectedDays <- generateLastNDays 40
  let actual = Set.fromList actualDays

  if expectedDays == actualDays then putStrLn ""
  else do
    time <- getTime
    putStrLn $ makeDateString time $ firstMissing expectedDays actual

getDayString :: String -> Set.Set String -> String
getDayString expected actuals =
  if Set.member expected actuals then "X" else "_"

printBashGui :: IO ()
printBashGui = do
  gui <- getBashGui
  putStrLn gui

getBashGui :: IO String
getBashGui = do
  actualDays <- getLastNGitCommitDays 10
  expectedDays <- generateLastNDays 10
  let actual = Set.fromList actualDays
  return $ foldl (\acc next -> acc ++ getDayString next actual) "" expectedDays

updateGitBashGui :: IO ()
updateGitBashGui = do
  gui <- getBashGui
  writeFile "/home/jack/programming/haskell-course/guifile" gui

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse ["-c"] = getOldestMissing
parse ["-b"] = printBashGui
parse ["-u"] = updateGitHooks
parse ["-w"] = updateGitBashGui
parse [] = usage >> exit

usage :: IO ()
usage   = putStrLn "Usage: dateParser \n [-v version]\n [-h help]\n [-c find oldest missing]\n [-b print bash gui] \n [-w write bash file] \n [-u update all git hooks]"
version :: IO ()
version = putStrLn "Haskell dateParser 1.0"
exit :: IO a
exit    = exitSuccess
die :: IO a
die     = exitWith (ExitFailure 1)


  -- let actualDays = ["2015-04-08","2015-04-09","2015-04-10","2015-04-11" ,"2015-04-12","2015-04-13","2015-04-14","2015-04-15","2015-04-16","2015-04-17","2015-04-18","2015-04-19","2015-04-20","2015-04-21","2015-04-22","2015-04-23","2015-04-24","2015-04-25","2015-04-26","2015-04-27"]
  -- let expectedDays = ["2015-04-08","2015-04-09","2015-04-10","2015-04-11","2015-04-12","2015-04-13","2015-04-14","2015-04-15","2015-04-16","2015-04-17","2015-04-18","2015-04-19","2015-04-20","2015-04-21","2015-04-22","2015-04-23","2015-04-24","2015-04-25","2015-04-26","2015-04-27"]
