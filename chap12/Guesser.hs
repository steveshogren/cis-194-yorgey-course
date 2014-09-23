import Data.List 

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Hand = TwoKind FaceValue
          | ThreeKind FaceValue
          | FourKind FaceValue
          | HighCard
          | Flush Suit
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

groupFn f = groupBy (\c1 c2 -> (==) (f c1) (f c2))  

grouped = groupFn fv . sort 

-- ofAKind 2 h1
ofAKind num h = filter (\x -> length x > (num - 1)) $ grouped h

flush x =
  let g = groupFn s x
  in (length (head g) == 5, g)

identify x =
  let t4 = ofAKind 4 x
      t3 = ofAKind 3 x
      t2 = ofAKind 2 x
      (isFlush, f1) = flush x
  in if isFlush then
       Flush $ s . head . head $ f1
     else if length t4 > 0 then
            FourKind (fv $ head $ head t4)
          else if length t3 > 0 then
                 ThreeKind (fv $ head $ head t3)
               else if length t2 > 0 then
                      TwoKind (fv $ head $ head t2)
                    else HighCard
  
h1 = parseHand "H6 H2 D2 D8 C9"
h2 = parseHand "H2 H2 D2 D8 C9"
h3 = parseHand "H2 H2 D2 D2 C9"
h4 = parseHand "H2 H5 H7 H10 H9"

rt = 
    identify h1 == TwoKind 2 &&
    identify h2 == ThreeKind 2 &&
    identify h3 == FourKind 2 &&
    identify h4 == Flush Hearts

