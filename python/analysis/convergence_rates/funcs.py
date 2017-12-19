from itertools import *
import matplotlib.pyplot as plt
from math import factorial
import numpy as np
import os
clear = lambda: os.system('cls')
clear()

def plot_convergence_rates(x,y,x_label,y_label,fig_title):

	p = -1; y1_linear = p*np.log(x)
	p = -2; y2_linear = p*np.log(x)
	p = -3; yn_linear = p*np.log(x)

	y1_linear = y1_linear - np.mean(y1_linear) + np.mean(np.log(y)) # Vertically shift to (x,y)
	y2_linear = y2_linear - np.mean(y2_linear) + np.mean(np.log(y)) # Vertically shift to (x,y)
	yn_linear = yn_linear - np.mean(yn_linear) + np.mean(np.log(y)) # Vertically shift to (x,y)

	y = y
	y1 = np.exp(y1_linear)
	y2 = np.exp(y2_linear)
	yn = np.exp(yn_linear)

	fs = 14

	# ---------------------------------------------------------------
	Lx = np.log(x)
	Ly1 = np.log(y1)
	Ly2 = np.log(y2)
	Lyn = np.log(yn)
	Ly = np.log(y)

	slopes = np.diff(Ly)/np.diff(Lx)
	mean_slope = np.mean(slopes)
	print('mean slope = '+str(mean_slope))

	# figure
	# plot(Lx,Ly,'b-+'); hold on
	# plot(Lx,Ly2,'r-',Lx,Ly1,'g-')
	# title(fig_title,'fontsize',fs)
	# xlabel(x_label,'fontsize',fs)
	# ylabel(y_label,'fontsize',fs)
	# AX=legend('Result','slope=-2','slope=-1')
	# LEG = findobj(AX,'type','text')
	# set(LEG,'FontSize',fs)
	# ---------------------------------------------------------------
	plt.figure
	plt.plot(Lx,Ly,'b-+')
	plt.title(fig_title+' for best curve fit')
	plt.xlabel(x_label)
	plt.ylabel(y_label)
	plt.legend('Result')
	plt.legend()
	plt.show()
	# ---------------------------------------------------------------
	x = np.exp(Lx)
	y1 = np.exp(Ly1)
	y2 = np.exp(Ly2)
	yn = np.exp(Lyn)
	y = np.exp(Ly)
	plt.figure
	plt.loglog(x,y,'b-+')
	plt.loglog(x,y1,'g-')
	plt.loglog(x,y2,'r-')
	# plt.loglog(x,yn,'k-')
	plt.title(fig_title)
	plt.xlabel(x_label)
	plt.ylabel(y_label)
	L = ['Result']
	L = L+['slope=-1']
	L = L+['slope=-2']
	# L = L+['slope=-n']
	print(L)
	# plt.legend(L[0],L[1],L[2],L[3])
	plt.legend(L)
	plt.show()
	# plt.legend(L[0],L[1],L[2])
	# AX=legend(L[0],L[1],L[2])
	# AX=legend('Result','slope=-2','slope=-1');
	# AX=legend('Result','slope=-1','slope=-2','slope=-1');
	# AX=legend('Result','slope=-1','slope=-2','slope=-1');
	# LEG = findobj(AX,'type','text')
	# set(LEG,'FontSize',fs)
	# ---------------------------------------------------------------
