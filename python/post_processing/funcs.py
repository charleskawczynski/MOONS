import numpy as np
import os
import math as m
from os import listdir
from os.path import isfile, join
import pylab
import matplotlib.pyplot as plt
import math as m
import numpy as np
from shutil import copyfile
import sys

def get_dt(file_name,n):
	fp = open(file_name)
	for i, line in enumerate(fp):
		if i == n:
			TS = line[11:]
			break
	fp.close()
	T = np.fromstring(TS, sep=' ')
	dt = T[1]
	return dt

def print_error(e):
	for k in range(0,5): print '*************************************'
	print e
	for k in range(0,5): print '*************************************'

def get_and_check_dt(root):
	dt_mom = get_dt(root+'parameters\\info_mom.dat',5)
	dt_ind = get_dt(root+'parameters\\info_ind.dat',4)
	if dt_ind != dt_mom: print_error('Time steps are not equal.')
	else: dt = dt_mom
	return dt

def make_path(newpath):
	if not os.path.exists(newpath):
		os.makedirs(newpath)

def directory_tree(root,newpath,PS):
	path = root+newpath
	path = path.replace(root,'')
	p = path.split(PS)
	p = filter(None, p) # Remove empty strings
	l = []
	for k in p:
		l.append(k+PS)
		new_path = root+''.join(l)
		make_path(new_path)

def highest_matching_directory(f1,f2,PS):
	p1 = f1.split(PS); p1 = filter(None,p1)
	p2 = f2.split(PS); p2 = filter(None,p2)
	l = []
	for k1,k2 in zip(p1,p2):
		if k1==k2: l.append(k1+PS)
		else: mismatch=k1; break
	return (''.join(l),mismatch)

def convert_N_to_t(root,LDC,file_old,file_new,header_var_suffix,ext):
	dt = get_and_check_dt(LDC)
	convert_N_to_t_given_dt(root,file_old+ext,file_new+ext,header_var_suffix,dt)

def convert_N_to_t_given_dt(root,file_old,file_new,header_var_suffix,dt):
	arr = np.loadtxt(file_old,skiprows=3)
	t = arr[:,0]*dt # Convert to t
	arr[:,0] = t
	s = arr.shape
	N_desired_points = 300
	n_skip = m.ceil(s[0]/N_desired_points)
	arr = arr[0::n_skip,:] # Sample output
	H = get_header(file_old) # Adjust header
	TF = False
	if TF: # Make header specific
		H[1] = H[1][:-1]+'_'+header_var_suffix+H[1][-1]
		H[1] = H[1][0:12] + H[1][11:].replace('=','')
		H[1] = H[1].replace('N','t_'+header_var_suffix)
	else:
		H[1] = H[1].replace('N','t')
	head = ''.join(H)
	delim = '	  '
	copyfile(file_old,file_new)
	np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file
	print 'Converted old:'+file_old.replace(root,'')
	print 'Converted new:'+file_new.replace(root,'')

def get_header(file_name):
	fp = open(file_name)
	H = []
	for i, line in enumerate(fp):
		if i < 3: H.append(line)
		else: break
	fp.close()
	return H

def copy_file(src,dst):
	copyfile(src,dst)

def copy_data_file(file_old,file_new):
	arr = np.loadtxt(file_old,skiprows=3) # Get data
	H = get_header(file_old) # Get header
	head = ''.join(H)
	delim = '	  '
	np.savetxt(file_new, arr, delimiter=delim, header = head, comments='') # Save file

def get_data(f):
	arr = np.loadtxt(f,skiprows=3) # Get data
	H = get_header(f) # Get header
	head = ''.join(H)
	return (arr,head)

def compute_difference_same_grid(f1,f2):
	(a_1,h_1) = get_data(f1)
	(a_2,h_2) = get_data(f2)
	h = h_1.replace('PV',''); h = h.replace('RV','')
	s1 = a_1.shape; s2 = a_2.shape
	(interior,total) = get_interior_data_set(a_1,a_2)
	(d,a,e) = compare_data_set(interior,total)
	return (d,h,a,e)

def compare_data_set(interior,total):
	d = total
	d[:,3:] = d[:,3:] - interior[:,3:] # PV - RV
	d[:,3:] = np.abs(d[:,3:])		  # |PV - RV|
	E = 0.5*d[:,3:]*d[:,3:]		 # Energy error
	d_amax_abs = np.amax(np.abs(d[:,3:]))
	E_amax_abs = np.amax(np.abs(E))
	return (d,d_amax_abs,E_amax_abs)

def export_difference_same_grid(f1,f2,diff_dir,diff_name,file_name,PS):
	(arr,head,a,e) = compute_difference_same_grid(f1,f2)
	delim = '	  '
	np.savetxt(diff_dir, arr, delimiter=delim, header = head, comments='') # Save file
	return (a,e)

def get_interior_data_set(a1,a2):
	s1 = a1.shape; s2 = a2.shape
	if (s1[0]>s2[0]): interior = get_interior_data_set_sorted(a1,a2); total = a2
	else:			 interior = get_interior_data_set_sorted(a2,a1); total = a1
	return (interior,total)

def get_interior_data_set_sorted(a1,a2): # len(a1)>len(a2)
	interior = []; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				interior.append(a1[i,:])
				k=k+1
	return np.array(interior)

def get_vec_data(d): return ([k[0] for k in d],[k[1] for k in d])

def get_vec_data_np(d): return (np.array([k[0] for k in d]),np.array([k[1] for k in d]))

def plot_all_files_in_path(file_path,x_label,y_label,PS):
	onlyfiles = [k for k in listdir(file_path) if isfile(join(file_path, k))]
	ymax = []; xmax = []
	for filename in onlyfiles:
		file = file_path+PS+filename
		(arr,header) = get_data(file)
		(x,y) = get_vec_data_np(arr)
		ymax.append(np.fabs(y))
		xmax.append(np.fabs(x))
		# xmax.append(np.fabs(x[len(x)-1]))
		plt.plot(x,y,label=filename.replace('LDC_',' ').replace('.dat','').replace('_',','))
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


def compare_PV_RV(root,source,target,v,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	PV_RV_abs_error = []; PV_RV_abs_error_name = []
	PV_RV_energy_error = []
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = highest_matching_directory(PVt,RVt,PS)
		# print 'PVs='+PVs.replace(root,''); print 'PVt='+PVt.replace(root,'')
		# print 'RVs='+RVs.replace(root,''); print 'RVt='+RVt.replace(root,'')
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')+'_diff_'+v+'np.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print 'diff_dir='+diff_dir.replace(root,'')
		print 'diff_name='+diff_name
		(a,e) = export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name,diff_name,v+'np',PS)
		PV_RV_abs_error.append(a)
		PV_RV_energy_error.append(e)
		d = diff_dir+diff_name
		PV_RV_abs_error_name.append(d.replace(root,''))

	for n,e,err in zip(PV_RV_abs_error_name,PV_RV_energy_error,PV_RV_abs_error):
		print 'dir,error,energy_diff='+n,err,e

def compare_PV_RV_1D(root,source,target,var,PS):
	return


def copy_KE_ME_to_common_folder(root,source,target,fileName,variable,PS):
	for s,t in zip(source,target):
		f_src = t+variable+'field'+PS+fileName
		# Filename processing
		file_name = t+fileName
		file_name = file_name.replace(root,'').replace(PS,'_').replace(' ','')
		file_name = file_name.replace('LDC'+PS,'').replace('PP_','').replace('.dat','')
		print 'file_name='+file_name
		f_dst = root+'PP'+PS+file_name+'.dat'
		print 'f_src:'+f_src.replace(root,'')
		print 'f_dst:'+f_dst.replace(root,'')
		copy_file(f_src,f_dst)

def convert_N_to_t_all(root,source,target,PS):
	for s,t in zip(source,target):
		f_src = s+'Ufield'+PS+'KU.dat'
		f_dst = t+'KU_vs_t.dat'
		print 'f_src:'+f_src.replace(root,'')
		print 'f_dst:'+f_dst.replace(root,'')
		convert_N_to_t(root,s,s+'Ufield'+PS+'KU'	,t+'Ufield'+PS+'KU_vs_t'	,'vs_t','.dat')
		convert_N_to_t(root,s,s+'Bfield'+PS+'KBi_f' ,t+'Bfield'+PS+'KBi_f_vs_t' ,'vs_t','.dat')
		convert_N_to_t(root,s,s+'Bfield'+PS+'KBi_c' ,t+'Bfield'+PS+'KBi_c_vs_t' ,'vs_t','.dat')
		convert_N_to_t(root,s,s+'Bfield'+PS+'KBi'   ,t+'Bfield'+PS+'KBi_vs_t'   ,'vs_t','.dat')

def make_directory_tree_target(root,source,target,PS):
	for s,t in zip(source,target):
		directory_tree(root,t+'Ufield'+PS,PS)
		directory_tree(root,t+'Bfield'+PS,PS)
		directory_tree(root,t+'Jfield'+PS,PS)
		directory_tree(root,t+'Tfield'+PS,PS)
		directory_tree(root,t+'material'+PS,PS)
		directory_tree(root,t+'parameters'+PS,PS)
