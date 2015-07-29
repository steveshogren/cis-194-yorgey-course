
data Suit = Clubs | Spades | Hearts | Diamonds
          deriving (Eq, Ord, Show)

type FaceValue = Int

data Hand = HighCard Card
          | TwoKind FaceValue
          | TwoPair FaceValue FaceValue
          | ThreeKind FaceValue
          | Straight FaceValue
          | Flush Suit
          | FullHouse FaceValue FaceValue
          | FourKind FaceValue
          | StraightFlush FaceValue Suit
          deriving (Eq, Ord, Show)

data Card = Card { fv :: FaceValue, s :: Suit } 
          deriving (Eq, Show, Ord)

parse :: String -> Hand
parse = HighCard $ Card 3 Spades

main :: IO ()
main = print $ parse "H2 D4 C3 S8 S9"
