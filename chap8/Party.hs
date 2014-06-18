-- module Party
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons em (GL emps funs) = GL (em : emps) $ funs + empFun em 
