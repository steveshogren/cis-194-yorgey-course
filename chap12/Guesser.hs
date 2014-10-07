import Data.List 
import Control.Monad
import Data.Maybe

empty = (==0) . length
isSeq = not . empty

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Eq, Ord, Show)

type FaceValue = Int

data Hand = HighCard 
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

parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

parseNum "A" = 1
parseNum x = read x

-- parse "H2"
parse str = Card  (parseNum $ tail str)  (parseSuit $ head str)

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

-- replace 1 2 [1,2] 
replace :: Eq a => a -> a -> [a] -> [a]
replace needle with  [] = []  
replace needle with (x:tail) =
  if x == needle then
    with : replace needle with tail
  else x : replace needle with tail


straight cs =
  let fs = faces cs
      start = head fs
      expected = [start..start+4]
  in (expected==fs, (head . reverse) fs)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: ([Card] -> Bool) -> Parser [Card]
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

isFlushF x = 
  let (isFlush, f1) = flush x
  in if isFlush then
       Just $ Flush f1
     else Nothing

is4KF x =
  let (is4K, t4) = ofAKind 4 x
  in if is4K then
       Just $ FourKind t4
     else Nothing
is3KF x =
  let (is3K, t3) = ofAKind 3 x
  in if is3K then
       Just $ ThreeKind t3
     else Nothing


identify :: [Card] -> Hand
identify x =
  let (is4K, t4) = ofAKind 4 x
      (is3K, t3) = ofAKind 3 x
      (is2K, t2) = ofAKind 2 x
      (isFlush, f1) = flush x
      (isStraightL, s1) = straight x
      (isStraightH, s2) = straight $ replace (Card 1 Hearts) (Card 14 Hearts) x
      isStraight = isStraightL || isStraightH
      s = if isStraightL then s1 else s2
  in if isFlush && isStraight then
       StraightFlush s f1
     else if isFlush then
            Flush f1
          else if is4K then
                 FourKind t4
               else if isStraight then
                      Straight s
                    else if is3K && is2K then
                           FullHouse t3 t2 
                         else if is3K then
                                ThreeKind t3
                              else if is2K then
                                     TwoKind t2
                                   else HighCard

winner :: [Hand] -> Hand
winner = head . reverse . sort
  
k2 = identify $ parseHand "H6 H2 D2 D8 C9"
k3 = identify $ parseHand "H2 H2 D2 D8 C9"
k4 = identify $ parseHand "H2 H2 D2 D2 C9"
fl = identify $ parseHand "H2 H5 H7 H10 H9"
st = identify $ parseHand "H6 S3 H4 D5 H2"
stal = identify $ parseHand "HA S3 H4 D5 H2"
stah = identify $ parseHand "HA S10 H13 D12 H11"
fh = identify $ parseHand "H2 H2 D2 D9 C9"
stfl = identify $ parseHand "H6 H3 H4 H5 H2"
twoP = identify $ parseHand "H6 D6 H2 H5 H2"
twoPC = parseHand "H6 D6 H2 H5 H2"

rt = 
    k2 == TwoKind 2 &&
    k3 == ThreeKind 2 &&
    k4 == FourKind 2 &&
    fl == Flush Hearts &&
    fh == FullHouse 2 9 && -- three 2s higher
    st == Straight 6 &&
    stal == Straight 5 &&
    stah == Straight 14 &&
    stfl == StraightFlush 6 Hearts
    && winner [k3, k2] == k3
    && winner [k3, fl, k2] == fl
    && winner [TwoKind 3, TwoKind 10] == TwoKind 10


