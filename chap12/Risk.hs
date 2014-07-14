{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Applicative
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Eq, Ord, Show)

attackRollCount 0          = 0
attackRollCount a | a <= 3 = a
attackRollCount _          = 3

defendRollCount 0          = 0
defendRollCount d | d <= 2 = d
defendRollCount d          = 2

data Winner = Attacker | Defender
  deriving (Eq, Ord, Show)

fight1on1 (ar, dr) = if unDV(ar) <= unDV(dr)
                     then Defender
                     else Attacker

getRollCounts a d = (,) <$> attackRollCount(a) <*> defendRollCount(d)

-- oneFight battlefield = 
--   let a = attackers battlefield
--       d = defenders battlefield
--       attackRolls = sort $ rolls(attackRollCount a)
--       defendRolls = sort $ rolls(defendRollCount d)
--   in attackRolls

rolls :: Army -> Rand StdGen [DieValue]
rolls 0 = return []
rolls count = do
  hroll <- die
  trolls <- rolls (count - 1)
  return (hroll : trolls)

-- oneFight battlefield = 
--   let a = attackers battlefield
--       d = defenders battlefield
--       attackRolls = sort $ rolls(attackRollCount a)
--       defendRolls = sort $ rolls(defendRollCount d)
--   in attackRolls

-- battle :: Battlefield -> Rand StdGen Battlefield
-- battle b =
--   let a = attackers b
--       d = defenders b
--   in if a > 1 && b > 0  

roll = (evalRandIO die)
printBattle b = putStrLn("Results: " ++ show(b))

roller4 = 
  fmap (\n ->
         let i = unDV n
         in Battlefield {attackers=i, defenders=i}) roll 
  >>= printBattle

roller3 = 
  fmap (+10) (evalRandIO die)
  >>= (\s -> putStrLn("Roll: " ++ show(unDV(s))))

roller2 = 
  evalRandIO die
  >>= (\s -> putStrLn("Roll: " ++ show(10+unDV(s))))

roller = do
  v <- evalRandIO die
  let t = unDV v
    in putStrLn ("Roll: " ++ show t )

  
