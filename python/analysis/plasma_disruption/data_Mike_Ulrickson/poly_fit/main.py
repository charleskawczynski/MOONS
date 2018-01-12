import sys
import os
import file_IO as IO
from scipy import optimize
import matplotlib.pyplot as plt
import numpy as np
from os import listdir
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

def read_file_to_list(file_name):
	with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
		L = f.read().splitlines() # list of lines
	return L

t = read_file_to_list('time.dat')
B_p = read_file_to_list('B_poloidal_mean.dat')
t = [float(x) for x in t]
B_p = [float(x) for x in B_p]
i_start = 8
i_stop = len(t)
t = [t[i] for i in range(i_start,i_stop)]
B_p = [B_p[i] for i in range(i_start,i_stop)]
t = np.array(t)
B_p = np.array(B_p)


def piecewise_linear(x, x0, y0, k1, k2):
    return np.piecewise(x, [x < x0], [lambda x:k1*x + y0-k1*x0, lambda x:k2*x + y0-k2*x0])

p , e = optimize.curve_fit(piecewise_linear, t, B_p)
xd = np.linspace(0, 15, 100)
plt.plot(t, B_p, "o")
plt.plot(xd, piecewise_linear(xd, *p))
plt.show()

IO.delete_pyc_files()
