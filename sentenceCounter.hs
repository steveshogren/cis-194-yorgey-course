import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

data Stats = Stats { occurences :: Int, sentencess :: [SentenceNumber] }
  deriving (Eq, Ord)

instance Show Stats where
  show Stats { occurences = oc, sentencess = s} = show oc ++ ":" ++ show s

type SentenceNumber = Int
type Sentence = String
type Word = String
type Concordance = Map Word Stats

sentences :: String -> [Sentence]
sentences s = map T.unpack $ T.splitOn (T.pack ".") (T.pack s)

mergeStats :: Stats -> Stats -> Stats
mergeStats Stats { sentencess=newSen } 
  Stats { occurences=oldO, sentencess=oldSen } =
  Stats { occurences=oldO+1, sentencess= oldSen++newSen}

-- m = addWordToConc "test" 2 Map.empty
-- m2 = addWordToConc "test" 4 m
addWordToConc :: Word -> SentenceNumber -> Concordance -> Concordance
addWordToConc word sNum conc =
  Map.insertWith mergeStats word Stats {occurences=1, sentencess=[sNum]} conc

addSentenceToConc :: Sentence -> SentenceNumber -> Concordance -> Concordance
addSentenceToConc s sNum conc =
  foldl (\c -> \word -> addWordToConc word sNum c) conc $ words s

newConc = (1, Map.empty)

-- makeCon "Hello there. Hello."
-- fromList [("Hello",2:[1,2]),("there",1:[1])]
makeCon :: String -> Concordance
makeCon input =
  snd . foldl (\(cnt, conc) -> \sent ->
                ((cnt+1), addSentenceToConc sent cnt conc)) newConc $ sentences input 


