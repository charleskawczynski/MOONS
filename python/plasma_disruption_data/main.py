import sys
import os
import file_IO as IO
import numpy as np
import re
import matplotlib.pyplot as plt
from os import listdir
from os.path import isfile, join
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.animation as animation
import types
import xlrd
clear = lambda: os.system('cls')
clear()
PS = '\\'
plot_transient_L = False
plot_transient_0D_L = False
export_3D_L = False
export_1D_L = False
export_mean_1D_L = True

# ************************************ GET DIRECTORIES **************************************
base_dir = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Dropbox'+PS+'UCLA'+PS+'RESEARCH'+PS+'Documents_exchange_Sergey'+PS+'Plasma_disruption_data_from_Ulrickson'+PS
B_r_data_dir = base_dir+'data_email_6'+PS # Br
B_z_data_dir = base_dir+'data_email_7'+PS # Bz
print ' ----------------------------------------------- ';
print 'Data files:'
print ''
def atoi(text):return int(text) if text.isdigit() else text
def natural_keys(text):return [ atoi(c) for c in re.split('(\d+)', text) ]
def get_data(data_dir):
	files = IO.get_all_files_in_path(data_dir)
	data_files = [x for x in files if x.endswith('.xlsx')]
	data_files.sort(key=natural_keys)
	time = data_files
	data_files = [data_dir+x for x in data_files]
	time = [x.replace('md_up_lin36_','').replace('.xlsx','') for x in time]
	time = [re.sub("\D", "", x) for x in time]
	T = map(int, time)
	T.sort()
	return (T,data_files)
(T,B_r_data_files) = get_data(B_r_data_dir)
(T,B_z_data_files) = get_data(B_z_data_dir)
for x in B_r_data_files: print x.replace(base_dir,'')
for x in B_z_data_files: print x.replace(base_dir,'')

# ************************************ DATA **************************************
# Rgrid: 830,835,840,845,850,855,860,865,870,875,880
# Zgrid: -100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100
# print worksheet.cell(updown, leftright).value # cell index starts at top left
N_r = 11
N_z = 21
N_t = len(T)
R = range(0,N_r)
Z = range(0,N_z)
r_phys = [830,835,840,845,850,855,860,865,870,875,880]
z_phys = [-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,0,10,20,30,40,50,60,70,80,90,100]
# ******************************** COLLECT DATA **********************************
B_r = np.zeros((N_r,N_z,N_t))
B_z = np.zeros((N_r,N_z,N_t))
B_mean_vs_time = np.zeros(N_t)
for t in range(0,N_t):
	worksheet = xlrd.open_workbook(B_r_data_files[t]).sheet_by_index(0)
	for r in R:
		for z in Z:
			B_r[r,z,t] = -worksheet.cell(r, z).value
for t in range(0,N_t):
	worksheet = xlrd.open_workbook(B_z_data_files[t]).sheet_by_index(0)
	for r in R:
		for z in Z:
			B_z[r,z,t] = -worksheet.cell(r, z).value

for t in range(0,N_t):
	Bmag = np.sqrt(B_r[:,:,t]**2.0+B_z[:,:,t]**2.0)
	B_mean_vs_time[t] = np.sum(Bmag)/(N_r*N_z)

# ************************** Time Dependent Animation ****************************
if plot_transient_L:
	fig = plt.figure()
	ims = []
	X,Y = np.meshgrid(r_phys, z_phys)
	ax = fig.gca(projection='3d')
	for i in range(0,N_t):
		t_step = int(i)
		Z = B_r[:,:,i].transpose()
		im = ax.plot_surface(X, Y, Z, rstride=1, cstride=1, linewidth=0, antialiased=False)
		def setvisible(self,vis):
			for c in self.collections: c.set_visible(vis)
		im.set_visible = types.MethodType(setvisible,im)
		im.axes = plt.gca()
		im.figure=fig
		ims.append([im])
	ani = animation.ArtistAnimation(fig, ims, interval=70, blit=False,repeat_delay=1000)
	plt.show()

# ************************** Time Dependent Plot ****************************
if plot_transient_0D_L:
	n_r = 4
	n_z = 10
	x = T
	y_R = B_r[n_r,n_z,:]
	y_Z = B_z[n_r,n_z,:]
	plt.plot(x,y_R,x,y_Z)
	plt.ylabel('B [T]')
	plt.xlabel('time [ms]')
	plt.title('B vs t at r = '+str(r_phys[n_r])+' and z = '+str(z_phys[n_z]))
	loc = 'lower right'
	loc = 'upper right'
	plt.legend(['B_r','B_z'],loc)
	plt.show()

# ****************************** Export 2D Time Dependent ********************************

if export_3D_L:
	d = '\t \t'
	file_name = 'B'
	export_dir = '3D'+PS
	file = export_dir+file_name
	for t in range(0,N_t):
		L = []
		t_s = str('%02d' % (t,))
		print t_s
		for z in Z:
			for r in R:
				L.append(str(r_phys[r])+d+str(z_phys[z])+d+str(B_r[r,z,t])+d+str(B_z[r,z,t]))
		header = []
		header.append('TITLE = "B vs R and Z (Ulrickson)"')
		header.append('VARIABLES = "r","z","B_r","B_z"')
		header.append('ZONE , T ="'+t_s+'", I = '+str(N_r)+', J = '+str(N_z)+' DATAPACKING = POINT \n')
		f = open(file+'_t='+t_s+'.dat', "w")
		f.write('\n'.join(header))
		f.write('\n'.join(L))
		f.close()

# ************************** Export Location Time dependent ******************************

if export_1D_L:
	d = '\t \t'
	n_r = 4
	n_z = 10
	B_str_post = '(r='+str(r_phys[n_r])+' cm,z='+str(z_phys[n_z])+' cm) [T]'
	B_str = 'B'+B_str_post
	B_r_str = 'B_r'
	B_z_str = 'B_z'
	T_str = 't [micro s]'
	file_name = B_str+' vs '+T_str
	export_dir = '1D'+PS
	file = export_dir+file_name
	L = []
	for t in range(0,N_t):
		t_s = str('%02d' % (t,))
		L.append(str(T[t])+d+str(B_r[n_r,n_z,t])+d+str(B_z[n_r,n_z,t]))
	header = []
	header.append('TITLE = "'+B_str+' vs '+T_str+' (Ulrickson)"')
	header.append('VARIABLES = "'+T_str+'","'+B_r_str+'","'+B_z_str+'"')
	header.append('ZONE, I = '+str(N_t)+' DATAPACKING = POINT \n')
	f = open(file+'.dat', "w")
	f.write('\n'.join(header))
	f.write('\n'.join(L))
	f.close()

if export_mean_1D_L:
	d = '\t \t'
	B_str = 'B_mean [T]'
	T_str = 't [micro s]'
	file_name = B_str+' vs '+T_str
	export_dir = '1D'+PS
	file = export_dir+file_name
	L = []
	for t in range(0,N_t):
		t_s = str('%02d' % (t,))
		L.append(str(T[t])+d+str(B_mean_vs_time[t]))
	header = []
	header.append('TITLE = "'+B_str+' vs '+T_str+' (Ulrickson)"')
	header.append('VARIABLES = "'+T_str+'","'+B_str+'"')
	header.append('ZONE, I = '+str(N_t)+' DATAPACKING = POINT \n')
	f = open(file+'.dat', "w")
	f.write('\n'.join(header))
	f.write('\n'.join(L))
	f.close()

IO.delete_pyc_files()
