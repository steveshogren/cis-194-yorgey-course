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

main :: IO()
main = do
  -- let t = withRepository lgFactory ((parent . fromText) "/home/jack/programming/sicp/.git") False
  hout <- readProcess "git" ["log", "-3", "--pretty=format:\"%at\""] "/home/jack/programming/sicp"
  let x =  splitOn "\n" hout
  let n = map (epochToClockTime . toInt . (replace "\"" "")) x
  print n
  -- let x = traverseCommits 
  
  -- let path =  "/home/jack/programming/"
  -- onlyGits <- find always (fileType ==? Directory &&? fileName ==? ".git") path
  -- print onlyGits
