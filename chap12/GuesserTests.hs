module GuesserTests where

import Data.Either
import Control.Monad
import Guesser

k2 = identify $ parseHand "H6 H2 D2 D8 C9"
k3 = identify $ parseHand "H2 H2 D2 D8 C9"
k4 = identify $ parseHand "H2 H2 D2 D2 C9"
fl = identify $ parseHand "H2 H5 H7 H10 H9"

winnerRunner hands expected =
  case (winner (lefts hands), expected) of
   (actual, Left expected) -> actual == expected
   _ -> False

ast :: (String, Hand) -> [String] -> [String]
ast (actual, expected) state =
  let actu = (identify (parseHand actual))
      result = actu == (Left expected)
  in if result
     then state
     else ("FAILED  Actual: " ++ (show actu) ++ " -> Expected: " ++ (show expected)) : state 
  
rt =
  foldr ast [] [("H6 H2 D2 D8 C9", (TwoKind 2)),
                ("H2 H2 D2 D8 C9", (ThreeKind 2)),
                ("H10 H2 D3 D8 C9", (HighCard $ Card 10 Hearts)),
                ("H2 H2 D2 D2 C9", (FourKind 2)),
                ("H2 H2 D9 D9 C3", (TwoPair 2 9)),
                ("H2 H5 H7 H10 H9", (Flush Hearts)),
                ("H2 H2 D2 D9 C9", (FullHouse 2 9)),  -- three 2s higher
                ("H6 S3 H4 D5 H2", (Straight 6)) ,
                ("HA S3 H4 D5 H2", (Straight 5)),
                ("HA S10 H13 D12 H11", (Straight 14)),
                ("H6 H3 H4 H5 H2", (StraightFlush 6 Hearts))]

rot = winnerRunner [k3, k2] k3
      && winnerRunner [k3, fl, k2] fl
      && winner [TwoKind 3, TwoKind 10] == TwoKind 10
