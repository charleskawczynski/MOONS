# from sympy import E, Eq, Function, pde_separate, Derivative as D
# from sympy.abc import x, t
from __future__ import division
from sympy import *
import numpy as np
import os
clear = lambda: os.system('cls')
clear()

x, y, z, t = symbols('x y z t')
# k, m, n = symbols('k m n', integer=True)
# f, g, h = symbols('f g h', cls=Function)
# fy = Derivative(f, y)

f = Function('f')
u = f(x, y)
ux = u.diff(x)
uy = u.diff(y)
eq = Eq(1 + (2*(ux/u)) + (3*(uy/u)))
s = pdsolve(eq)

# u, X, T = map(Function, 'uXT')
# eq = Eq(D(u(x, t), x), E**(u(x, t))*D(u(x, t), t))
# pde_separate(eq, u(x, t), [X(x), T(t)], strategy='add')

# eq = Eq(D(u(x, t), x, 2), D(u(x, t), t, 2))
# s = pde_separate(eq, u(x, t), [X(x), T(t)], strategy='mul')

print s
