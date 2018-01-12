import sys
import os
import file_IO as IO
import numpy as np
import re
from matplotlib import pyplot
from os import listdir
from os.path import isfile, join
import types
clear = lambda: os.system('cls')
clear()
PS = '\\'

def write_list_to_file(file_name,L):
	f = open(file_name,'w+')
	f.write('\n'.join(L))
	f.close()

def get_file_contents(file_name):
	with open(file_name, 'r') as content_file: contents = content_file.read()
	return contents.split('\n')

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

temp = np.polyfit(t,B_p,3)
t_poly = np.linspace(np.amin(t),np.amax(t))
B_poly = np.poly1d(temp)

pyplot.plot(t,B_p,'r-x',t_poly,B_poly(t_poly),'b-x')
pyplot.title('Poly Fit Data')
pyplot.xlabel('Time')
pyplot.ylabel('B_poloidal_mean')
pyplot.legend(['Original Data','Poly Fit'])
pyplot.show()



IO.delete_pyc_files()
