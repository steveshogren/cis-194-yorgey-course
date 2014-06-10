{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- import ExprT
import StackVM
import Parser

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
-- eval :: ExprT -> Integer
-- eval (Lit x) = x
-- eval (Mul x y) = (eval x) * (eval y)
-- eval (Add x y) = (eval x) + (eval y)


-- evalStr "(2+3)*4" == Just 20
-- evalStr "(2+3)*" == Nothing
-- evalStr :: String -> Maybe Integer
-- evalStr s =
--   case parseExp Lit Add Mul s of
--     Just x -> Just $ eval x
--     Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


-- instance Expr ExprT where
--   lit = Lit
--   add = Add
--   mul = Mul

-- reify :: ExprT -> ExprT
-- reify = id

instance Expr Integer where
  lit = id
  add = (+) 
  mul = (*)

instance Expr Bool where
  lit a = if a > 0 then True else False
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a+b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a+b) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- data StackExp = PushI Integer
--               | PushB Bool
--               | Add
--               | Mul
--               | And
--               | Or
--                 deriving Show


instance Expr Program where
  lit a = [PushI a]
  add [a] [b] = [a, b, Add]
  mul [a] [b] = [a, b, Mul]

-- convertProg :: ExprT -> 

-- compile :: String -> Maybe Program
-- compile s = 
--   case parseExp Lit Add Mul s of
--     Just x -> Just $ eval x
--     Nothing -> Nothing
