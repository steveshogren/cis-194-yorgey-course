import Data.List 

empty = (==0) . length
isSeq = not . empty

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Hand = TwoKind FaceValue
          | ThreeKind FaceValue
          | FourKind FaceValue
          | HighCard
          | Flush Suit
          | Straight FaceValue
          | StraightFlush FaceValue Suit
          | FullHouse FaceValue FaceValue
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
ofAKind num h =
  let kindGroup = filter (\x -> length x == num) $ grouped h
  in (isSeq kindGroup, (fv . head . head) kindGroup)


flush x =
  let g = groupFn s x
  in (length (head g) == 5, (s . head . head) g)

faces = map fv . sort

straight cs =
  let fs = faces cs
      start = head fs
      expected = [start..start+4]
  in (expected==fs, (head . reverse) fs)

identify x =
  let (is4K, t4) = ofAKind 4 x
      (is3K, t3) = ofAKind 3 x
      (is2K, t2) = ofAKind 2 x
      (isFlush, f1) = flush x
      (isStraight, s1) = straight x
  in if isFlush && isStraight then
       StraightFlush s1 f1
     else if isFlush then
            Flush f1
          else if is4K then
                 FourKind t4
               else if isStraight then
                      Straight s1
                    else if is3K && is2K then
                           FullHouse t3 t2 
                         else if is3K then
                                ThreeKind t3
                              else if is2K then
                                     TwoKind t2
                                   else HighCard

winner :: [Hand] -> Hand
winner = head . sort
  
k2 = parseHand "H6 H2 D2 D8 C9"
k3 = parseHand "H2 H2 D2 D8 C9"
k4 = parseHand "H2 H2 D2 D2 C9"
fl = parseHand "H2 H5 H7 H10 H9"
st = parseHand "H6 S3 H4 D5 H2"
fh = parseHand "H2 H2 D2 D9 C9"
stfl = parseHand "H6 H3 H4 H5 H2"

rt = 
    identify k2 == TwoKind 2 &&
    identify k3 == ThreeKind 2 &&
    identify k4 == FourKind 2 &&
    identify fl == Flush Hearts &&
    identify fh == FullHouse 2 9 && -- three 2s higher
    identify st == Straight 6 &&
    identify stfl == StraightFlush 6 Hearts 
--    winner [k3, k2] == k3 &&
--    winner [k3, fl, k2] == fl


