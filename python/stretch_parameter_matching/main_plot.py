from math import factorial
import numpy as np
import pylab
import matplotlib.pyplot as plt
import os
clear = lambda: os.system('cls')
clear()

with open('container/pre.txt', 'r') as content_file: pre = content_file.read()
with open('container/post.txt', 'r') as content_file: post = content_file.read()
with open('container/eq_start.txt', 'r') as content_file: eq_start = content_file.read()
with open('container/eq_end.txt', 'r') as content_file: eq_end = content_file.read()

b = np.linspace(1+1e-5,2,1000)
a = 0.5
dh = 8.6074170056442689e-3
hmin = -1.05
hmax = -1

t2 = 1
t1 = 0.5
g = (b+1)/(b-1)


ht2 = ((b+2*a)*g**t2 - b + 2*a)/((2*a + 1)*(1+g**t2))
ht1 = ((b+2*a)*g**t1 - b + 2*a)/((2*a + 1)*(1+g**t1))

h = ht2 - ht1 - dh/(hmax-hmin)

plt.plot(b,h)
plt.xlabel('beta')
plt.ylabel('h')
plt.title('h vs beta')
plt.show()

plt.plot(b,g)
plt.xlabel('beta')
plt.ylabel('g')
plt.title('g vs beta')
plt.show()
