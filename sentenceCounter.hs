import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

data Stats = Stats { occurences :: Int, sentencess :: [Int] }
  deriving (Eq, Ord)

instance Show Stats where
  show Stats { occurences = oc, sentencess = s} = show oc ++ ":" ++ show s

type Concordance = Map String Stats

sentences :: String -> [String]
sentences s = map T.unpack $ T.splitOn (T.pack ".") (T.pack s)

mergeStats :: Stats -> Stats -> Stats
mergeStats Stats { sentencess=newSen } 
  Stats { occurences=oldO, sentencess=oldSen } =
  Stats { occurences=oldO+1, sentencess= oldSen++newSen}

-- m = addToConcordance "test" 2 Map.empty
-- m2 = addToConcordance "test" 4 m
addToConcordance :: String -> Int -> Concordance -> Concordance
addToConcordance word sNum conc =
  Map.insertWith mergeStats word Stats {occurences=1, sentencess=[sNum]} conc

sentToConc :: String -> Concordance -> Int -> Concordance
sentToConc s conc sNum =
  foldl (\c -> \word -> addToConcordance word sNum c) conc $ words s

newConc = (1, Map.empty)

-- makeCon "Hello there. Hello."
-- fromList [("Hello",2:[1,2]),("there",1:[1])]
makeCon :: String -> Concordance
makeCon input =
  snd . foldl (\(cnt, conc) -> \sent ->
                ((cnt+1), sentToConc sent conc cnt)) newConc $ sentences input 

