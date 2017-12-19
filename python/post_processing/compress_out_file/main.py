# from sympy.solvers import solve
# from sympy import Symbol
# from sympy import *
from itertools import *
from math import factorial
import numpy as np
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

data_files = []
for subdir, dirs, files in os.walk('./'):
	for f in files:
		if f.endswith('.dat'): data_files=data_files+[f]

# data_files = [x for x in data_files if x=='data.dat']
for f in data_files:
	print(f)
	(d,h) = IO.get_data(f,3)
	h = IO.neglect_direction_in_header(h,3)
	print('h')
	print(h)
	h = IO.keep_component_only_in_header(h,3)
	d = np.delete(d,[2,3,4],axis=1)
	IO.save_data_to_file(f,d,h)


IO.delete_pyc_files()
