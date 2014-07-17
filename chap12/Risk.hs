{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
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

attackRollCount = min 3
defendRollCount = min 2

b = Battlefield {attackers=4, defenders=4}

getKills [] = (0,0) 
getKills ((at1,def1):res) = 
  let (atSum, defSum) = getKills res
-- never need to extract unDV... it is Ord
  in if def1 >= at1 then (atSum+1,defSum) else (atSum, defSum+1)

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield {attackers=att, defenders=def}) = 
  if ((att < 2) || (def == 0)) then
    return b
  else do
    bs <- oneFight b
    invade bs

oneFight :: Battlefield -> Rand StdGen Battlefield 
oneFight Battlefield {attackers=att, defenders=def} = do
  attackRolls <- sortRolls $ rolls(attackRollCount att)
  defendRolls <- sortRolls $ rolls(defendRollCount def)
  let (atkLosses, defLosses) = getKills $ zip attackRolls defendRolls
  return Battlefield {attackers=att-atkLosses, defenders=def-defLosses}

showFight :: IO () 
showFight = 
  (evalRandIO $ invade b)
  >>= printBattle

rolls :: Army -> Rand StdGen [DieValue]
rolls 0 = return []
rolls count = do
  hroll <- die
  trolls <- rolls (count - 1)
  return (hroll : trolls)

sortRolls :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] 
sortRolls = liftM $ sortBy (flip compare)  

battle :: Battlefield -> Rand StdGen Battlefield
battle = oneFight

roll :: IO DieValue
roll = (evalRandIO die)

printBattle :: Show a => a -> IO ()
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

  
