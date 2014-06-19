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

glCons :: Employee -> GuestList -> GuestList
glCons em (GL emps funs) = GL (em : emps) $ funs + empFun em 

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2

instance Monoid GuestList where
  mempty  = GL [] 0
  mappend g1@(GL es1 f1) g2@(GL es2 f2) = GL (es1 ++ es2) $ f1 + f2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acum (Node{rootLabel=l,subForest=[]}) = f l acum    
