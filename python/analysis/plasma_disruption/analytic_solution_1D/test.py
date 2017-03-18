from sympy import E, Eq, Function, pde_separate, Derivative as D
from sympy.abc import x, t
u, X, T = map(Function, 'uXT')

eq = Eq(D(u(x, t), x), E**(u(x, t))*D(u(x, t), t))
pde_separate(eq, u(x, t), [X(x), T(t)], strategy='add')
[exp(-X(x))*Derivative(X(x), x), exp(T(t))*Derivative(T(t), t)]

