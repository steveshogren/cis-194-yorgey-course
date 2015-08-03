module LookupGitDirs (getGitDirectories) where

import System.FilePath.Find (FindClause, find, fileName, depth, (<=?), (==?), (/~?), (&&?), filePath)
import Control.Monad

getIgnoredList :: IO [String]
getIgnoredList = liftM words $ readFile "/home/jack/programming/haskell-course/gitignores"

addIgnored :: FindClause Bool -> [String] -> FindClause Bool
addIgnored = foldl (\res next -> res &&? (filePath /~? "**" ++ next ++"**"))

filterFor :: [String] -> FindClause Bool
filterFor = addIgnored (fileName ==? ".git") 

getGitDirectories :: IO [FilePath]
getGitDirectories = do
  let path =  "/home/jack/programming/"
  ignores <- getIgnoredList
  onlyGits <- find (depth <=? 3) (filterFor ignores) path
  return $ onlyGits ++ ["/home/jack/.emacs.bak/.git", "/home/jack/private/.git"]
