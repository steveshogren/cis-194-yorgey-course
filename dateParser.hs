import System.FilePath
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import Data.Char
import qualified Data.Text as T

codeDirectory = "/home/jack/programing"

searchPath = codeDirectory ++ "/**/.git"


main = do
--  all <- getDirectoryContents "/home/jack/programming"
  all <- getDirectoryContents "/home/jack/programming/sicp"
  let filtered = filter (isPrefixOf ".git") all
  print filtered


