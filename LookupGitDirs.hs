module LookupGitDirs (getGitDirectories) where

import System.FilePath.Find (FindClause, find, fileName, depth, (<=?), (==?), (/~?), (&&?), filePath)

filterIgnored :: [String]
filterIgnored = ["convincing-coworkers"]

addIgnored :: FindClause Bool -> FindClause Bool
addIgnored appendTo = foldl (\res next -> res &&? (filePath /~? "**" ++ next ++"**"))
                      appendTo filterIgnored

filterFor :: System.FilePath.Find.FindClause Bool
filterFor = addIgnored (fileName ==? ".git") 

getGitDirectories :: IO [FilePath]
getGitDirectories = do
  let path =  "/home/jack/programming/"
  onlyGits <- find (depth <=? 3) filterFor path
  return $ onlyGits ++ ["/home/jack/.emacs.bak/.git", "/home/jack/private/.git"]
