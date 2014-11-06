import Data.List 
import Data.Maybe
import Data.Either
import Control.Monad

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

getOfKindType 2 = TwoKind
getOfKindType 3 = ThreeKind
getOfKindType 4 = FourKind

ofAKind :: Int -> [Card] -> Maybe Hand
ofAKind num h =
  let kindGroup = filter (\x -> length x == num) $ grouped h
      nums = (fv . head . head) kindGroup
  in if (isSeq kindGroup)
     then Just(getOfKindType num $ nums)
     else Nothing

flush :: [Card] -> Maybe Hand
flush x =
  let g = groupFn s x
      isFlush = length (head g) == 5
      suit = (s . head . head) g
  in if isFlush then Just (Flush suit) else Nothing

faces = map fv . sort

-- replace 1 2 [1,2] 
replace :: Eq a => a -> a -> [a] -> [a]
replace needle with  [] = []  
replace needle with (x:tail) =
  if x == needle then
    with : replace needle with tail
  else x : replace needle with tail

straightL :: [Card] -> Maybe Hand
straightL cs =
  let fs = faces cs
      start = head fs
      expected = [start..start+4]
  in if (expected==fs)
     then Just $ Straight $ (head . reverse) fs
     else Nothing

straight :: [Card] -> Maybe Hand          
straight cs =
  case (straightL cs, straightL $ replace (Card 1 Hearts) (Card 14 Hearts) cs) of
  (Just s, _) -> Just s
  (_, Just s) -> Just s
  _ -> Nothing
                                 
doE :: ([Card] -> Maybe Hand) -> [Card] -> Either Hand [Card] 
doE f c = 
    case f c of
      Nothing   -> Right c
      Just hand -> Left hand

fourK = ofAKind 4
threeK = ofAKind 3
twoK = ofAKind 2

fullHouse :: [Card] -> Maybe Hand
fullHouse h =
  case (threeK h, twoK h) of
   (Just(ThreeKind thf), Just(TwoKind tf)) -> Just $ FullHouse thf tf
   _ -> Nothing

straightflush :: [Card] -> Maybe Hand
straightflush h =
  case (straight h, flush h) of
   (Just(Straight s), Just(Flush f)) -> Just $ StraightFlush s f
   _ -> Nothing

identify :: [Card] -> Either Hand [Card] 
identify x =
  Right x >>=
  (doE straightflush) >>=
  (doE flush) >>=
  (doE fourK) >>=
  (doE straight) >>=
  (doE fullHouse) >>=
  (doE threeK) >>=
  (doE twoK)

winner :: [Hand] -> Hand
winner = head . reverse . sort
  
k2 = identify $ parseHand "H6 H2 D2 D8 C9"
k3 = identify $ parseHand "H2 H2 D2 D8 C9"
k4 = identify $ parseHand "H2 H2 D2 D2 C9"
fl = identify $ parseHand "H2 H5 H7 H10 H9"

winnerRunner hands expected =
  case (winner (lefts hands), expected) of
   (actual, Left expected) -> actual == expected
   _ -> False

ast actual expected =
  (identify (parseHand actual)) == (Left expected)
  
rt =
    ast "H6 H2 D2 D8 C9" (TwoKind 2)
    && ast "H2 H2 D2 D8 C9" (ThreeKind 2)
    && ast "H2 H2 D2 D2 C9" (FourKind 2)
    && ast "H2 H5 H7 H10 H9" (Flush Hearts)
    && ast "H2 H2 D2 D9 C9" (FullHouse 2 9)  -- three 2s higher
    && ast "H6 S3 H4 D5 H2" (Straight 6) 
    && ast "HA S3 H4 D5 H2" (Straight 5)
    && ast "HA S10 H13 D12 H11" (Straight 14) 
    && ast "H6 H3 H4 H5 H2" (StraightFlush 6 Hearts)
    && winnerRunner [k3, k2] k3
    && winnerRunner [k3, fl, k2] fl
    && winner [TwoKind 3, TwoKind 10] == TwoKind 10

