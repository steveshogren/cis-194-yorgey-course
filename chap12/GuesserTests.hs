module GuesserTests where

import Data.Either
import Control.Monad
import Guesser

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

k2 = TwoKind 2
k3 = ThreeKind 2
k4 = FourKind 2
fl = Flush Hearts

rot = winner [k3, k2] == k3
      && winner [k3, fl, k2] == fl
      && winner [TwoKind 3, TwoKind 10] == TwoKind 10
