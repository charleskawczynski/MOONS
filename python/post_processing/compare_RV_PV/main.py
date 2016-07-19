import os
import funcs as f
clear = lambda: os.system('cls')
clear()

from os import listdir
from os.path import isfile, join
import pylab
import matplotlib.pyplot as plt
import math as m
import numpy as np


mypath = 'C:\Users\Charlie\Documents\MOONS\Numerical Experiment Matrix'

onlyfiles = [k for k in listdir(mypath) if isfile(join(mypath, k))]

print onlyfiles

ymax = 0
for filename in onlyfiles:
	file = mypath+'\\'+filename
	# print file
	# f = open(file,'r')
	# header = f.readlines()[0:3]
	# data = f.readlines()[4:]
	(arr,header) = f.get_data(file)
	(x,y) = f.get_vec_data(arr)
	ymax = np.fabs(y)
	ymax = np.amax(ymax)

	print filename

	plt.plot(x,y,label=filename[3:-4])


plt.xlabel('Time')
plt.ylabel('Total Kinetic Energy')
plt.title('Total Kinetic Energy vs. Time')

plt.legend(loc=4,prop={'size':10})
plt.axis([0, 40, 0, ymax*1.2])
# plt.legend(loc='upper left')

plt.show()

print 'Done'

