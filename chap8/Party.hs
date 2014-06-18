-- module Party
import Employee

gl = GL [] 0
stan = (Emp "Stan" 4)

glCons :: Employee -> GuestList -> GuestList
glCons em (GL emps funs) = GL (em : emps) $ funs + empFun em 


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 > f2 then g1 else g2
