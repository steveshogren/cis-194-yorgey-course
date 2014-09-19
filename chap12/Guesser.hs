data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Card = Card Suit FaceValue 
  deriving (Eq, Show)

parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

parse str = Card (parseSuit $ head str) (read $ tail str) 

-- parse "H2"


