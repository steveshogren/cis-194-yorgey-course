import System.FilePath
import System.Directory
import Data.String.Utils
import Control.Monad
import System.FilePath.Find

main :: IO()
main = do
  let path =  "/home/jack/programming/"
  onlyGits <- find always (fileType ==? Directory &&? fileName ==? ".git") path
  print onlyGits
