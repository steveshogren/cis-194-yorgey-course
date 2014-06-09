import ExprT
import Parser

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)


-- evalStr "(2+3)*4" == Just 20
-- evalStr "(2+3)*" == Nothing
evalStr :: String -> Maybe Integer
evalStr s =
  case parseExp Lit Add Mul s of
    Just x -> Just $ eval x
    Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
         lit = Lit
         add = Add
         mul = Mul

reify :: ExprT -> ExprT
reify = id
