
from sympy import *
x = Symbol('x')
y = Symbol('y')
z = Symbol('z')



#z = solve(x+y+2,x)
#print z

a = Eq(x + 5*y, 2)
b = Eq(-3*x + 6*y, 15)
z = solve([a, b], [x, y])

print z


