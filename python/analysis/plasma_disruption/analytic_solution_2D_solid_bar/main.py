import os
clear = lambda: os.system('cls')
clear()
import numpy as np
import math
from matplotlib import pyplot
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import axes3d
import matplotlib.animation as animation
from matplotlib import cm

# ***************************************************************
# *************************************************************** PARAMS / INIT
# ***************************************************************

def read_file_to_list(file_name):
	with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
		L = f.read().splitlines() # list of lines
	return L

def get_mesh(x,y):
	X,Y = np.meshgrid(x,y)
	return (X,Y)

def get_params():
	L_x = 1.0
	L_y = 1.0
	L_x2i = 1.0/L_x**2.0
	L_y2i = 1.0/L_y**2.0
	R = 1.0
	pi2R = math.pi**2.0*R
	coeff = 4.0/(math.pi**2.0)
	return (L_x,L_y,L_x2i,L_y2i,pi2R,R,coeff)

def get_t_all():
	t_all = np.array([float(x) for x in read_file_to_list('time.dat')])
	t_c = 0.0047
	t_all = t_all*10.0**(-6.0)
	t_all = t_all/t_c
	return t_all

def get_2D_field_steady(Nx,Ny):
	return np.empty([Nx,Ny])*0.0

def get_2D_field_unsteady(Nx,Ny,Nt):
	return np.empty([Nx,Ny,Nt])*0.0

def get_B_all():
	B_all = np.array([float(x) for x in read_file_to_list('B_poloidal.dat')])
	return B_all

def get_coordinates(t_all):
	(L_x,L_y,L_x2i,L_y2i,pi2R,R,coeff) = get_params()
	quick_run = True
	# quick_run = False
	L_t = t_all[-1]
	L_t = t_all[-6]
	if quick_run:
		L_t = t_all[4]
	x = np.linspace(0.0, L_x, 10)
	y = np.linspace(0.0, L_y, 10)
	t = np.linspace(0.0, L_t, 1000)
	if quick_run:
		t = np.linspace(0.0, L_t, 30)
	dt = t[1]-t[0]
	Nx = len(x)
	Ny = len(y)
	Nt = len(t)
	return (x,y,t,Nx,Ny,Nt,dt)

def interp_simple(B1,t1,B2,t2,t):
	return B1 + (B2-B1)*(t-t1)/(t2-t1)
def get_dB0_dt_from_t(t_all,B_all,t):
	n_data_points = 58-1
	i_start = 8-1
	time = t+t_all[i_start]
	dB0_dt = (B_all[1]-B_all[0]) / (t_all[1]-t_all[0])
	n = len(t_all)-1
	for i in range(0,n-1):
		if ((time >= t_all[i]) and (time <= t_all[i+1])):
			dB0_dt = (B_all[i+1]-B_all[i])/(t_all[i+1]-t_all[i])
	if (time >= t_all[n]):
		dB0_dt = 0.0
	return dB0_dt
def comput_theta_t_data(t_all,B_all,t):
	return -get_dB0_dt_from_t(t_all,B_all,t)

def comput_theta_t(t_all,B_all,t):
	return comput_theta_t_data(t_all,B_all,t)

def comput_theta_t_constant(t_all,B_all,t):
	return 1.0

# ***************************************************************
# *************************************************************** INTEGRAL EVALUATION
# ***************************************************************

def compute_spatial_factor(G,n,m,x,y,L_x,L_y,Nx,Ny):
	if n==0: n_temp = 0.0
	else: n_temp = 1.0/n
	if m==0: m_temp = 0.0
	else: m_temp = 1.0/m
	coeff = (1.0 - (-1.0)**n)* \
			(1.0 - (-1.0)**m)* \
			n_temp* \
			m_temp
	for i in range(0,Nx):
		for j in range(0,Ny):
			G[i,j] = math.sin(n*math.pi*x[i]/L_x)*math.sin(m*math.pi*y[j]/L_y)*coeff
	return G

def compute_time_dependent_factor_mem(pi2R,t_start,t_stop,n,m,L_x2i,L_y2i,t_all,B_all,N_tau):
	Tau = np.linspace(t_start,t_stop,N_tau)
	dtau = Tau[1]-Tau[0]
	TDF = 0.0
	for i in range(0,len(Tau)):
		tau = Tau[i]
		TDF += math.exp(-pi2R*(t_stop-tau)*(L_x2i*n**2.0*+L_y2i*m**2.0))*comput_theta_t(t_all,B_all,tau)*dtau
	return TDF

def compute_all_time_dependent_factors():
	(L_x,L_y,L_x2i,L_y2i,pi2R,R,coeff) = get_params()
	N_tau = 10
	t_all = get_t_all()
	B_all = get_B_all()
	(x,y,t,Nx,Ny,Nt,dt) = get_coordinates(t_all)
	temp_xy = get_2D_field_steady(Nx,Ny)
	TDF = get_2D_field_unsteady(Nx,Ny,Nt)
	N_iter_x = 3
	N_iter_y = 3
	for k in range(0,Nt-1):
		t_start = t[k]
		t_stop = t[k+1]
		for n in range(1,N_iter_x):
			for m in range(1,N_iter_y):
				temp_xy = compute_spatial_factor(temp_xy,n,m,x,y,L_x,L_y,Nx,Ny)
				temp_t = compute_time_dependent_factor_mem(pi2R,t_start,t_stop,n,m,L_x2i,L_y2i,t_all,B_all,N_tau)
				TDF[:,:,k] += temp_xy*temp_t
	return TDF

# ***************************************************************
# *************************************************************** POST-PROCESSING
# ***************************************************************

def plot_2D_surface(X,Y,Z,x_label,y_label,tit):
	fig = plt.figure()
	ax = fig.gca(projection='3d')
	ax.plot_surface(X, Y, Z,cmap=cm.viridis)
	plt.title(tit)
	plt.xlabel(x_label)
	plt.ylabel(y_label)
	plt.show()

def plot_1D_func(x,y,x_label,y_label,tit):
	fig = plt.figure()
	ax = fig.gca()
	ax.plot(x,y)
	plt.title(tit)
	plt.xlabel(x_label)
	plt.ylabel(y_label)
	plt.show()

# ***************************************************************
# *************************************************************** ANIMATION ATTEMPS
# ***************************************************************

def update(k,ax,fig,X,Y,B,B_max):
	ax.cla()
	wframe = ax.plot_surface(X, Y, B[:,:,k],cmap=cm.viridis)
	ax.set_zlim(0,B_max)
	return wframe,

def animate_B_solution(B):
	t_all = get_t_all()
	B_all = get_B_all()
	(L_x,L_y,L_x2i,L_y2i,pi2R,R,coeff) = get_params()
	(x,y,t,Nx,Ny,Nt,dt) = get_coordinates(t_all)
	(X,Y) = get_mesh(x,y)
	fig = plt.figure()
	ax = axes3d.Axes3D(fig)
	B_max = np.amax(B)
	wframe = ax.plot_surface(X, Y, B[:,:,0],cmap=cm.viridis)
	ax.set_zlim(0,B_max)
	ani = animation.FuncAnimation(fig, update, frames=range(Nt), fargs=(ax,fig,X,Y,B,B_max), interval=Nt)
	plt.show()
	ani.save('basic_animation.mp4', fps=30, extra_args=['-vcodec', 'libx264'])
	# ani.save('animation.mp4', writer=animation.FFMpegWriter())
	# ani.save('animation.mp4')
	# ani.save('test.mp4') # Does not work...

# ***************************************************************
# *************************************************************** MAIN FUNCTIONS
# ***************************************************************

def plot_steady_B(B):
	t_all = get_t_all()
	B_all = get_B_all()
	(L_x,L_y,L_x2i,L_y2i,pi2R,R,coeff) = get_params()
	(x,y,t,Nx,Ny,Nt,dt) = get_coordinates(t_all)
	(X,Y) = get_mesh(x,y)
	plot_2D_surface(X,Y,B[:,:,-2],'x','y','B')

def plot_dBdt_vs_time():
	t_all = get_t_all()
	B_all = get_B_all()
	(x,y,t,Nx,Ny,Nt,dt) = get_coordinates(t_all)
	theta = [comput_theta_t_data(t_all,B_all,x) for x in t]
	plot_1D_func(t,theta,'Time','dB/dt','dB/dt vs time')

plot_dBdt_vs_time()
print('Done with plot_dBdt_vs_time')
B = compute_all_time_dependent_factors()
print('Done with compute_all_time_dependent_factors')
plot_steady_B(B)
print('Done with plot_steady_B')
animate_B_solution(B)
print('Done with animate_B_solution')
