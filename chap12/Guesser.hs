data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Card = Card Suit FaceValue 
  deriving (Eq, Show)

parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

-- parse "H2"
parse str = Card (parseSuit $ head str) (read $ tail str) 

parseHand = map parse . words 
h1 = parseHand "H2 D2 H5 D8 C9"




