module LookupGitDirs (getGitDirectories, getGits) where

import System.FilePath.Find (FindClause, find, fileName, depth, (<=?), (==?), (/~?), (&&?), filePath)
import Control.Monad
import Data.Maybe (catMaybes)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

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

listGitDirsWithCurrentChanges repoPath = do
  (code,stdout,_) <- readProcessWithExitCode "git" ["--git-dir", repoPath, "rev-list", "current", "^master", "--no-merges"] "."
  return $ if code == ExitSuccess then Just (repoPath,words stdout) else Nothing

getGits :: IO [(String, [String])]
getGits = do
  gits <- getGitDirectories
  dires <- mapM listGitDirsWithCurrentChanges gits
  return $ catMaybes dires

-- "git rev-parse --verify current && git lg current ^master --no-merges"
