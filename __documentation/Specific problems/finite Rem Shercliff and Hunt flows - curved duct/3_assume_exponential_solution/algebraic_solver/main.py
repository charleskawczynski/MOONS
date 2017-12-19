from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
clear = lambda: os.system('cls')
clear()


i = Symbol('i')
u = Symbol('u')
v = Symbol('v')
w = Symbol('w')
p = Symbol('p')
B_x = Symbol('B_x')
B_y = Symbol('B_y')
B_z = Symbol('B_z')

A = Symbol('A')
B = Symbol('B')
T = Symbol('T')

M = Symbol('M')
H = Symbol('H')
G = Symbol('G')
RI = Symbol('1/rho')
BBx = Symbol('BB_x')
UBx = Symbol('UB_x')

X = Symbol('\PD_x UB')
Y = Symbol('\PD_y UB')
Z = Symbol('\PD_z UB')
MX = Symbol('\PD_x BB')
MY = Symbol('\PD_y BB')
MZ = Symbol('\PD_z BB')

eq1 = -u*(G - i*T*UBx) -RI*i*T*p + E*B_x*M + E*(B_y*MY + B_z*MZ)
eq2 = -v*G             -RI*i*A*p + E*B_y*M
eq3 = -w*G             -RI*i*B*p + E*B_z*M
eq4 = -B_x*H + u*M +B_y*Y +B_z*Z + v*MY +w*MZ
eq5 = -B_y*H + v*M
eq6 = -B_z*H + w*M
eq7 = T*u +  A*v +  B*w
root = solve([eq1,eq2,eq3,eq4,eq5,eq6,eq7],[u,v,w,p,B_x,B_y,B_z])
print root

X = Symbol('X')
Y = Symbol('Y')
eq1 =  X + 1*Y - 5
eq2 =  X - 2*Y
root = solve([eq1,eq2],[X,Y])
print root

s = ''
content = []
BS = '\\'
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