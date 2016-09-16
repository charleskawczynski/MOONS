from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
clear = lambda: os.system('cls')
clear()


b = Symbol('beta')
a = Symbol('alpha')
t = Symbol('theta')
t = Symbol('theta')
gs = Symbol('\gamma')
g = (b+1)/(b-1)
# print g

hn = ((b+2*a)*g**t - b + 2*a)/((2*a + 1)*(1+g**t))

# print hn
root = diff(hn,b)
root = simplify(root)
s = latex(root.subs({(b+1)/(b-1):gs}))

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