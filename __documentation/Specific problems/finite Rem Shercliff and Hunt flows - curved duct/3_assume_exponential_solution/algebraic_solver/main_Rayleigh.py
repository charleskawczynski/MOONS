from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
clear = lambda: os.system('cls')
clear()

X = Symbol('X')
Y = Symbol('Y')
eq1 =  X + 1*Y - 5
eq2 =  X - 2*Y
root = solve([eq1,eq2],[X,Y])
print root
print X
print Y
print 'Solved simple system!'

# i = Symbol('i')
i = sqrt(-1)
l = Symbol('l')
m = Symbol('m')
s = Symbol('s')

u = Symbol('u')
v = Symbol('v')
w = Symbol('w')
rho = Symbol('\\rho ')
omega = Symbol('\\omega ')
R = Symbol('R')
n = Symbol('n')
theta = Symbol('\\theta ')
alpha = Symbol('\alpha ')
beta = Symbol('\\beta ')
# kappa = Symbol('\kappa ')
kappa = 0
gamma = Symbol('\gamma ')
eq1 = i*l*u + i*m*v + i*s*w
eq2 = n*u -(-1/rho*i*l*omega)
eq3 = n*v -(-1/rho*i*m*omega)
eq4 = n*w -(-1/rho*i*s*omega + gamma*theta)
eq5 = n*theta + w*beta - kappa*(-s**2-l**2 -m**2)*theta
root = solve([eq1,eq2,eq3,eq4,eq5],[n,u,v,w,theta])
print root
print ''
print root[0]
print ''
print root[1]
print ''
temp = root[0][0]
sol = latex(simplify(factor(temp)))
print 'SOLUTION FINISHED!'

s = str(sol).replace('*','')
content = []
BS = ''
content.append('\\begin{equation}')
content.append(s)
content.append('\\end{equation}')

with open('container/pre.txt', 'r') as content_file: pre = content_file.read()
with open('container/post.txt', 'r') as content_file: post = content_file.read()
f = open('tex/main.tex', "w")
f.write(pre)
f.write(''.join(content))
f.write('')
f.write(post)
f.close()
print 'Done'