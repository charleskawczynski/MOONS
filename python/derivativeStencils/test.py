
from sympy import *
x = Symbol('x')
y = Symbol('y')
z = Symbol('z')


#for i in range(1,10):
#    EQ(i) = Eq(x + 5*y, 2)

a = Eq(x + 5*y, 2)
print type(a)
b = Eq(-3*x + 6*y, 15)
z = solve([a, b], [x, y])

print z


