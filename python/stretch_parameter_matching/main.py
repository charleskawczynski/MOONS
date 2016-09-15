from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
clear = lambda: os.system('cls')
clear()

with open('container/pre.txt', 'r') as content_file: pre = content_file.read()
with open('container/post.txt', 'r') as content_file: post = content_file.read()
with open('container/eq_start.txt', 'r') as content_file: eq_start = content_file.read()
with open('container/eq_end.txt', 'r') as content_file: eq_end = content_file.read()


b = Symbol('b')
a = Symbol('a')
t = Symbol('t')
gs = Symbol('g')
g = (b+1)/(b-1)
# print g

ht = ((b+2*a)*g**t - b + 2*a)/((2*a + 1)*(1+g**t))
hn = ((b+2*a)*g - b + 2*a)/((2*a + 1)*(1+g))

# print hn
root = diff(hn,b)
root = root.subs({(b+1)/(b-1):gs})
root = collect(root,g)
s = latex(root)

root_t = diff(ht,b)
root_t = root_t.subs({(b+1)/(b-1):gs})
root_t = collect(root_t,g)
s_t = latex(root_t)

#-------------CORRECT
K1 = (-1 + g + (2*a + b)/(b-1) - ((1 + b)*(2*a + b))/(b-1)**2)/((1 + g)*(2*a+1))
K2 = -(((b-1)**(-1) - g**2)*(2*a - b + ((1 + b)*(2*a + b))/(b-1)))/(1 + g)**2/(2*a+1)
K3 = -(-1 + (g**t) + (g**(-1 + t)*(2*a + b)*((b-1)**(-1) - g**2)*t))/(1 + (g**t))/(2*a+1)
K4 = (g**(-1 + t)*((b-1)**(-1) - g**2)*(2*a - b + (g**t)*(2*a + b))*t)/((1 + (g**t))**2)/(2*a+1)

K1 = K1.subs({(b+1)/(b-1):gs}); K1 = collect(K1,g); s_K1 = latex(K1)
K2 = K2.subs({(b+1)/(b-1):gs}); K2 = collect(K2,g); s_K2 = latex(K2)
K3 = K3.subs({(b+1)/(b-1):gs}); K3 = collect(K3,g); s_K3 = latex(K3)
K4 = K4.subs({(b+1)/(b-1):gs}); K4 = collect(K4,g); s_K4 = latex(K4)

content = []
BS = '\\'
NL = '\n'
content.append(eq_start)
content.append(s+NL)
content.append(eq_end)

content.append('$h_t$')
content.append(eq_start)
content.append(s_t+NL)
content.append(eq_end)

content.append('K1')
content.append(eq_start)
content.append(s_K1+NL)
content.append(eq_end)

content.append('K2')
content.append(eq_start)
content.append(s_K2+NL)
content.append(eq_end)

content.append('K3')
content.append(eq_start)
content.append(s_K3+NL)
content.append(eq_end)

content.append('K4')
content.append(eq_start)
content.append(s_K4+NL)
content.append(eq_end)

f = open('tex/main.tex', "w")
f.write(pre)
f.write(''.join(content))
f.write('')
f.write(post)
f.close()
print 'Done'