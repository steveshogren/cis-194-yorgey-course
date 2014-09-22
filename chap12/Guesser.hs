import Data.List 

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Hand = TwoKind FaceValue | ThreeKind FaceValue | FourKind FaceValue | HighCard
  deriving (Eq, Ord, Show)

data Card = Card { fv :: FaceValue, s :: Suit } 
  deriving (Eq, Show, Ord)

parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

-- parse "H2"
parse str = Card  (read $ tail str)  (parseSuit $ head str)

parseHand = map parse . words 


grouped = groupBy (\c1 c2 -> (==) (fv c1) (fv c2) ) . sort 

ofAKind num h = filter (\x -> length x > (num - 1)) $ grouped h

identify x =
  if length (ofAKind 4 x) > 0 then
    FourKind 1
  else if length (ofAKind 3 x) > 0 then
    ThreeKind 1
  else if length (ofAKind 2 x) > 0 then
    TwoKind 1
  else HighCard
  
h1 = parseHand "H6 H2 D2 D8 C9"
h2 = parseHand "H2 H2 D2 D8 C9"
h3 = parseHand "H2 H2 D2 D2 C9"



