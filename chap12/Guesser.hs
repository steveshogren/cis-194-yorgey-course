module Guesser (winner, identify, parseHand, Suit(..), Hand(..), FaceValue(..), Card(..)) where

import Data.List 
import Data.Maybe
import Data.Either
import Control.Monad
import Helpers

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
                   
parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades

parseNum "A" = 1
parseNum x = read x

-- parse "H2"
parse str = Card  (parseNum $ tail str)  (parseSuit $ head str)

parseHand = map parse . words 

groupedByFv = groupEqual fv . sort 

getOfKindType 2 = TwoKind
getOfKindType 3 = ThreeKind
getOfKindType 4 = FourKind

ofAKind :: Int -> [Card] -> Maybe Hand
ofAKind num h =
  let kindGroup = filter (\x -> length x == num) $ groupedByFv h
      nums = (fv . head . head) kindGroup
  in if (isSeq kindGroup)
     then Just(getOfKindType num $ nums)
     else Nothing

twoPair :: [Card] -> Maybe Hand
twoPair h =
  let kindGroup = filter (\x -> length x == 2) $ groupedByFv h
  in if length kindGroup == 2 
     then
       let fnum = (fv . head . head) kindGroup
           snum = (fv . head . head . tail) kindGroup
       in Just(TwoPair fnum snum)
     else Nothing

flush :: [Card] -> Maybe Hand
flush x =
  let g = groupEqual s x
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

highCard :: [Card] -> Maybe Hand
highCard h =
  let highest = (head . reverse . sort) h
  in Just $ HighCard highest

identify :: [Card] -> Either Hand [Card] 
identify x =
  Right x >>=
  (doE straightflush) >>=
  (doE flush) >>=
  (doE fourK) >>=
  (doE straight) >>=
  (doE fullHouse) >>=
  (doE twoPair) >>=
  (doE threeK) >>=
  (doE twoK) >>=
  (doE highCard)

winner :: [Hand] -> Hand
winner = head . reverse . sort
  

