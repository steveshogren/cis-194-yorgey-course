import System.FilePath
import System.Directory
import Data.String.Utils
import Data.List.Utils
import Control.Monad
import System.FilePath.Find
import Filesystem.Path.CurrentOS
import Filesystem.Path
import System.Process
import Data.List.Split (splitOn)
import System.Time.Utils

toInt :: String -> Int
toInt = read

getCommitDates repoPath = do
  hout <- readProcess "git" ["log", "-3", "--pretty=format:\"%at\""] repoPath
  let x =  splitOn "\n" hout
  return $ map (epochToClockTime . toInt . replace "\"" "") x

main :: IO()
main = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth ==? 0 ||? depth ==? 1) (System.FilePath.Find.contains ".git") path
  -- times <- map getCommitDates onlyGits
  -- print times
  print onlyGits
