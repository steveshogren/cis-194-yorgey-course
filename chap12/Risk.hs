{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

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

-- battle :: Battlefield -> Rand StdGen Battlefield
-- battle b =
--   let a = attackers b
--       d = defenders b
-- in  

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

  
