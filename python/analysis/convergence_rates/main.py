# from sympy.solvers import solve
# from sympy import Symbol
# from sympy import *
from itertools import *
from math import factorial
import numpy as np
import funcs as f
import os
import sys
clear = lambda: os.system('cls')
clear()
sys.path.insert(0, '../../lib/')
import file_IO as IO
import inspect

# B = IO.get_file_contents('A.dat')
# all_functions = inspect.getmembers(IO, inspect.isfunction)
# for x in all_functions: print(x[0])

A = IO.get_data('A.dat',0)
A = A[0]
B = A

s = A.shape
c = 1
for i in range(0,s[0]):
	B[i,c] = B[i,c] - B[s[0]-1,c]
B = B[0:-1]

dt = B[:,0]
e = abs(B[:,1])
x_label = '1/dt'
y_label = 'L_2 error'
fig_title = 'Temporal Convergence Rates'

x = 1/dt
# x = dt
y = e

f.plot_convergence_rates(x,y,x_label,y_label,fig_title)

IO.delete_pyc_files()
