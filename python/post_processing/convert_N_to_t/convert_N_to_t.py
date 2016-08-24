import numpy as np
import math as m
import file_IO as IO
import pylab
import inspect
import sys
import matplotlib.pyplot as plt

def get_dt(file_name,n):
	T = get_floats_in_line_n(file_name,n,11)
	dt = T[1]
	return dt

def get_Re_Ha(LDC_dir,PS):
	T = get_floats_in_line_n(LDC_dir+'parameters'+PS+'info_mom.dat',3,7)
	Re = T[0]
	Ha = T[1]
	return (Re,Ha)

def get_Rem(LDC_dir,PS):
	T = get_floats_in_line_n(LDC_dir+'parameters'+PS+'info_ind.dat',3,16)
	Rem = T[0]
	return Rem

def get_line_n(file,n):
	f = open(file)
	for i, line in enumerate(f):
		if i == n:
			line_n = line
			break
	f.close()
	return line_n

def get_floats_in_line_n(file,n,n_skip_spaces):
	s = get_line_n(file,n)
	L = s[n_skip_spaces:].split(' ')
	L = [x for x in L if not x=='']
	L = [x for x in L if any(char.isdigit() for char in x)]
	L = [np.fromstring(x, sep=' ') for x in L]
	L = [x[0] for x in L]
	return L

def print_error(e):
	for k in range(0,5): print '*************************************'
	print e
	for k in range(0,5): print '*************************************'

def get_and_check_dt(LDC,PS):
	dt_mom = get_dt(LDC+'parameters'+PS+'info_mom.dat',5)
	dt_ind = get_dt(LDC+'parameters'+PS+'info_ind.dat',4)
	if dt_ind != dt_mom: print_error('Time steps are not equal.')
	else: dt = dt_mom
	return dt

def convert_N_to_t(root,LDC,file_old,file_new,header_var_suffix,ext,PS):
	dt = get_and_check_dt(LDC,PS)
	convert_N_to_t_given_dt(root,file_old+ext,file_new+ext,header_var_suffix,dt,1,1,1)

def convert_N_to_t_and_scale_energy(root,LDC,file_old,file_new,header_var_suffix,ext,PS,scale):
	dt = get_and_check_dt(LDC,PS)
	if (scale):
		(Re,Ha) = get_Re_Ha(LDC,PS)
		Rem = get_Rem(LDC,PS)
		convert_N_to_t_given_dt(root,file_old+ext,file_new+ext,header_var_suffix,dt,Re,Ha,Rem)
	else:
		convert_N_to_t_given_dt(root,file_old+ext,file_new+ext,header_var_suffix,dt,1,1,1)

def convert_N_to_t_given_dt(root,file_old,file_new,header_var_suffix,dt,Re,Ha,Rem):
	arr = np.loadtxt(file_old,skiprows=3)
	fact = Ha**2.0/Re/Rem
	t = arr[:,0]*dt # Convert to t
	e = arr[:,1]*fact # scale energy
	arr[:,0] = t
	arr[:,1] = e
	s = arr.shape
	N_desired_points = 300
	if s[0]>N_desired_points: n_skip = m.ceil(s[0]/N_desired_points)
	else:                     n_skip = 1
	arr = arr[0::n_skip,:] # Sample output
	H = IO.get_header(file_old) # Adjust header
	TF = False
	if TF: # Make header specific
		H[1] = H[1][:-1]+'_'+header_var_suffix+H[1][-1]
		H[1] = H[1][0:12] + H[1][11:].replace('=','')
		H[1] = H[1].replace('N','t_'+header_var_suffix)
	else:
		H[1] = H[1].replace('N','t')
	head = ''.join(H)
	delim = '	  '
	IO.copy_file(file_old,file_new)
	np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file
	print 'Old copy:'+file_old.replace(root,'')
	print 'New copy:'+file_new.replace(root,'')

def plot_all_files_in_path(root,file_path,x_label,y_label,var,PS):
	onlyfiles = IO.get_all_files_in_path(file_path)
	ymax = []; xmax = []
	if var=='U': onlyfiles = [x for x in onlyfiles if 'KE' in x]
	elif var=='B': onlyfiles = [x for x in onlyfiles if 'ME' in x]
	else: sys.error('Error: variable input must be U or B')
	print '\n'.join(onlyfiles)
	if onlyfiles==[]:
		raise ValueError('Error: no files in path in '+__name__+' in '+inspect.getfile(inspect.currentframe()))
	for file_name in onlyfiles:
		file = file_path+file_name
		print 'file='+file.replace(root,'')
		(arr,header) = IO.get_data(file)
		(x,y) = IO.get_vec_data_np(arr)
		ymax.append(np.fabs(y))
		xmax.append(np.fabs(x))
		# xmax.append(np.fabs(x[len(x)-1]))
		plt.plot(x,y,label=file_name.replace('LDC_',' ').replace('.dat','').replace('_',','))
	xmax = [item for sublist in xmax for item in sublist] # flatten xmax
	ymax = [item for sublist in ymax for item in sublist] # flatten ymax
	plt.xlabel(x_label)
	plt.ylabel(y_label)
	x_max = np.amax(np.array(xmax))
	y_max = np.amax(np.array(ymax))
	plt.title(x_label + ' vs. ' + y_label)
	plt.legend(loc=4,prop={'size':10})
	plt.axis([0, x_max, 0, y_max*1.2])
	# plt.legend(loc='upper left')
	plt.show()

def print_all_SS_energy(root,file_path,v,PS):
	onlyfiles = IO.get_all_files_in_path(file_path)
	ymax = []; xmax = []
	x_SS = []; y_SS = []; name_SS = []
	onlyfiles = [x for x in onlyfiles if not 'global_data.dat' in x]
	if onlyfiles==[]:
		raise ValueError('Error: no files in path in '+__name__+' in '+inspect.getfile(inspect.currentframe()))
	for file_name in onlyfiles:
		file = file_path+PS+file_name
		print file.replace(root,'')
		(arr,header) = IO.get_data(file)
		(x,y) = IO.get_vec_data_np(arr)
		x_SS.append(x[-1])
		y_SS.append(y[-1])
		name_SS.append(file_name.replace('LDC_',' ').replace('.dat','').replace('_',',').replace('KU','KE').replace(' ',''))
		ymax.append(np.fabs(y))
		xmax.append(np.fabs(x))
	for a,b,c in zip(name_SS,x_SS,y_SS): print a,'\t',c

def copy_KE_ME_to_common_folder(root,source,target,energy_path,file_name,variable,PS):
	for s,t in zip(source,target):
		f_src = t+variable+'field'+PS+file_name
		# file_name processing
		file = t+file_name
		file = file.replace(root,'').replace('LDC','').replace('PP','').replace('.dat','')
		if file.startswith(PS): file = file[1:] # must be after .replace(root,'')
		file = file.replace(PS+PS,PS).replace(PS,'_').replace(' ','')
		file = file.replace('PV','_PV_').replace('RV','_RV_').replace('__','_')
		f_dst = root+energy_path+file+'.dat'
		print 'f_src:'+f_src.replace(root,'')
		print 'f_dst:'+f_dst.replace(root,'')
		IO.copy_file(f_src,f_dst)

def convert_N_to_t_all(root,source,target,PS):
	for s,t in zip(source,target):
		f_src = s+'Ufield'+PS+'KE.dat'
		f_dst = t+'KE_vs_t.dat'
		print 'f_src:'+f_src.replace(root,'')
		print 'f_dst:'+f_dst.replace(root,'')
		convert_N_to_t_and_scale_energy(root,s,s+'Ufield'+PS+'KU'	 ,t+'Ufield'+PS+'KE_vs_t'	 ,'vs_t','.dat',PS,False)
		convert_N_to_t_and_scale_energy(root,s,s+'Bfield'+PS+'KBi_f' ,t+'Bfield'+PS+'MEi_f_vs_t' ,'vs_t','.dat',PS,True)
		convert_N_to_t_and_scale_energy(root,s,s+'Bfield'+PS+'KBi_c' ,t+'Bfield'+PS+'MEi_c_vs_t' ,'vs_t','.dat',PS,True)
		convert_N_to_t_and_scale_energy(root,s,s+'Bfield'+PS+'KBi'   ,t+'Bfield'+PS+'MEi_t_vs_t' ,'vs_t','.dat',PS,True)
