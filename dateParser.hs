import System.FilePath
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import Data.Char
import Control.Monad
import qualified Data.Text as T

-- codeDirectory = "/home/jack/programing"

-- searchPath = codeDirectory ++ "/**/.git"

listDirs :: [FilePath] -> IO [FilePath]
listDirs = filterM doesDirectoryExist

joinFN :: String -> String -> FilePath
joinFN p1 p2 = joinPath [p1, p2]

-- isDoDD f = not $ (endsWith "/." f) || (endsWith "/.." f)



main = do
  let path =  "/home/jack/programming/"
  all <- getDirectoryContents path
  --noDots <- filterM (return . isDoDD) (map (joinFN path) all)
  dirs <- listDirs $ map (joinFN path) all
  let filtered = filter (isPrefixOf ".git") dirs
  print dirs


