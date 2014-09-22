import Data.List 

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int


data Card = Card { fv :: FaceValue, s :: Suit } 
  deriving (Eq, Show, Ord)

parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

-- parse "H2"
parse str = Card  (read $ tail str)  (parseSuit $ head str)

parseHand = map parse . words 

h1 = parseHand "H6 H2 D2 D8 C9"

-- identify = groupBy (\c1 c2 -> (==) (fv c1) (fv c2) ) . sort 
identify = groupBy (\c1 c2 -> (== fv c1) $ fv c2 ) . sort 

g1 = identify h1



