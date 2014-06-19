module Party where

import Employee
import Data.Monoid
import Data.Tree

gl = GL [] 0
stan = (Emp "Stan" 4)
dave = (Emp "Dave" 1)
gl1 = glCons stan gl
gl2 = glCons dave gl

x = Node {rootLabel="Test", subForest=[] }
y = Node {rootLabel="Other", subForest=[x] }

glCons :: Employee -> GuestList -> GuestList
glCons em (GL emps funs) = GL (em : emps) $ funs + empFun em 

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend g1@(GL es1 f1) g2@(GL es2 f2) = GL (es1 ++ es2) $ f1 + f2

treeToList :: Tree a -> [a]
treeToList Node{rootLabel=l,subForest=subs} = l : (concat $ map treeToList subs)

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f init = (foldr f init) . treeToList
