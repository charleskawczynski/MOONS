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
u = Symbol('u')
v = Symbol('v')
w = Symbol('w')
p = Symbol('p')
B_x = Symbol('B_x')
B_y = Symbol('B_y')
B_z = Symbol('B_z')
B_z0 = Symbol('B_z0')

E = Symbol('E')
R = Symbol('R')
nu = Symbol('\\nu')
# K = Symbol('K')
K = 0
nu_m = Symbol('\\nu_m')
n = Symbol('n')
theta = Symbol('\\theta')
alpha = Symbol('\\alpha')
beta = Symbol('\\beta')

BBx = Symbol('BB_x')
UBx = Symbol('UB_x')

X = Symbol('PD_x UB')
Y = Symbol('PD_y UB')
Z = Symbol('PD_z UB')
MX = Symbol('PD_x BB')
MY = Symbol('PD_y BB')
MZ = Symbol('PD_z BB')

eq1 = n*B_x -( - nu_m*B_x*K +B_y *Y +B_z* Z - UBx*i*theta*B_x)
eq2 = n*B_y -( - nu_m*B_y*K -  UBx*i*theta*B_y)
eq3 = n*B_z -( - nu_m*B_z*K -  UBx*i*theta*B_z)
eq4 = theta*B_x + alpha*B_y + beta*B_z
root = solve([eq1,eq2,eq3,eq4],[B_x,B_y,B_z,n])
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