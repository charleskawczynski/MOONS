import itertools
import os
import ntpath
from operator import itemgetter
import math
import math_funcs as MF
import numpy as np
import funcs as f
from itertools import groupby
import matplotlib.pyplot as plt
import matplotlib
import pylab
import sys
import file_IO as IO

def marker_list():
	marker = itertools.cycle(('+-', '.-', 'o-', '*-'))
	# marker = itertools.cycle(('-', '--', '-.', '*-', ':'))
	return marker

def combine_plots(target_dir,all_plots,symbol_markers):
	all_plots = f.flatten_list(all_plots)
	AX = [k.axes for k in all_plots]
	LINE = [k.get_data() for k in all_plots]
	X = [k.get_xdata() for k in all_plots]
	Y = [k.get_ydata() for k in all_plots]
	X_lab = [k.get_xlabel() for k in AX][0]
	Y_lab = [k.get_ylabel() for k in AX][0]
	xRange = AX[0].get_xlim()
	yRange = AX[0].get_ylim()
	LH = [k.get_legend_handles_labels() for k in AX]
	LEG_L = [k[1] for k in LH]
	LEG_L = f.flatten_list(LEG_L)
	plt.figure()
	marker = marker_list()
	# for x,y,SM in zip(X,Y,symbol_markers): plt.plot(x,y,SM)
	for x,y in zip(X,Y): plt.plot(x,y,next(marker))
	plt.xlim(xRange)
	plt.ylim(yRange)
	plt.legend(LEG_L)
	plt.xlabel(X_lab)
	plt.ylabel(Y_lab)
	LEG_name = ','.join(LEG_L)
	file_name = LEG_name
	pylab.savefig(target_dir+file_name+'.png', bbox_inches='tight')
	print('Just saved '+file_name+'.png')
	plt.cla()
	plt.clf()
	plt.close()
	return plt

def extract_line_from_file(f,line_location,line_dir):
	(a,h) = IO.get_data(f)
	(a,line_location_actual) = get_nearest_line_data_set(a,line_location,line_dir)
	print('line_location_actual')
	print(line_location_actual)
	h = IO.keep_direction_only_in_header(h,line_dir)
	return (a,h)

def extract_nearest_line_from_file(f,line_location,line_dir):
	(a,h) = IO.get_data(f)
	i = get_center_indexes(a,h,line_location)
	a = get_nearest_line_data_set(arr,line_location,line_dir)
	h = IO.keep_direction_only_in_header(h,line_dir)
	return (a,h)

def get_nearest_line_data_set(arr,line_location,line_dir):
	if line_dir==1: a=[1,2]
	elif line_dir==2: a=[0,2]
	elif line_dir==3: a=[0,1]
	else: raise NameError('dir must = 1,2,3 in get_nearest_line_data_set.')
	if len(line_location)==3: raise NameError('line_location must be of size 2 in get_nearest_line_data_set.')
	L1_val = abs(arr[:,a[0]]-line_location[0])
	L2_val = abs(arr[:,a[1]]-line_location[1])
	L1 = L1_val==L1_val.min()
	L2 = L2_val==L2_val.min()
	L = [k1 and k2 for k1,k2 in zip(L1,L2)]
	d_1D_all = arr[L,:]
	c = d_1D_all[:,0:3]
	d_1D = d_1D_all[:,3:]
	s = np.concatenate((c,d_1D),axis=1)
	line_location_actual = [np.mean(s[:,a[0]]),np.mean(s[:,a[1]])]
	s = np.delete(s,a,axis=1)
	return (s,line_location_actual)

def get_3D_data_set_shape_original(d_1D):
	L = []
	tol = 1e-15
	for d in [0,1,2]:
		c = d_1D[:,d]
		consecutive_duplicates = [abs(c[i]-c[i+1])<tol for i,x in enumerate(c[0:-1])]
		duplicates_of_first = [abs(c[i]-c[0])<tol for i,x in enumerate(c)]

		print('---------------------------- start')
		print('d = '+str(d))
		print('c[0:4] = '+str(c[0:4]))
		print('consecutive_duplicates[0:4] = '+str(consecutive_duplicates[0:4]))
		if not any(consecutive_duplicates):
			print('consecutive_duplicates DO NOT exist')
			L1 = [i for i,x in enumerate(duplicates_of_first) if x]
			s_1D = L1[1]-L1[0]
			print('s_1D = '+str(s_1D))
		else:
			print('consecutive_duplicates exist')
			consecutive_duplicates_removed = [c[i] for i,x in enumerate(c[0:-1]) if not abs(c[i]-c[i+1])<tol]
			if not abs(c[-1]-c[-2])<tol: consecutive_duplicates_removed = consecutive_duplicates_removed+c[-1]
			print('consecutive_duplicates_removed[0:4] = '+str(consecutive_duplicates_removed[0:4]))
			c_mod = consecutive_duplicates_removed
			duplicates_of_first = [abs(c_mod[i]-c_mod[0])<tol for i,x in enumerate(c_mod)]
			print('duplicates_of_first[0:4] = '+str(duplicates_of_first[0:4]))

			sparse_falses = [i for i,x in enumerate(duplicates_of_first) if x]
			# sparse_falses = [not x for x in consecutive_duplicates]
			# print('sparse_falses[0:4] = '+str(sparse_falses[0:4]))
			# sparse_falses = [i for i,x in enumerate(sparse_falses) if x]
			print('sparse_falses[0:4] = '+str(sparse_falses[0:4]))
			L1 = sparse_falses
			s_1D = L1[1]-L1[0]
			print('s_1D = '+str(s_1D))
		print('---------------------------- end')
		L.append(s_1D)
	print('**************************************************')
	print('L for get_3D_data_set_shape')
	print(L)
	print(L[2]/L[1])
	print('**************************************************')
	return L

def get_3D_data_set_shape(d_1D):
	L = []
	tol = 1e-15
	for d in [0,1,2]: L.append(get_3D_data_set_shape_1D(d_1D[:,d]))
	s_total = d_1D.shape[0]
	if not L[0]: L[0] = int(s_total/(L[1]*L[2]))
	if not L[1]: L[1] = int(s_total/(L[0]*L[2]))
	if not L[2]: L[2] = int(s_total/(L[0]*L[1]))
	return L

def get_3D_data_set_shape_1D(c):
	tol = 1e-15
	consecutive_duplicates = [abs(c[i]-c[i+1])<tol for i,x in enumerate(c[0:-1])]
	duplicates_of_first = [abs(c[i]-c[0])<tol for i,x in enumerate(c)]
	if not any(consecutive_duplicates):
		L1 = [i for i,x in enumerate(duplicates_of_first) if x]
		try: s_1D = L1[1]-L1[0]
		except: s_1D = False
	else:
		consecutive_duplicates_removed = [c[i] for i,x in enumerate(c[0:-1]) if not abs(c[i]-c[i+1])<tol]
		s_1D = get_3D_data_set_shape_1D(consecutive_duplicates_removed)
	return s_1D

def get_center_indexes(d_1D,header,location):
	tol = 1e-15
	s = IO.get_shape(header)
	x_3D = d_1D[:,0].reshape(s[0],s[1],s[2],order='F')
	y_3D = d_1D[:,1].reshape(s[0],s[1],s[2],order='F')
	z_3D = d_1D[:,2].reshape(s[0],s[1],s[2],order='F')
	index_x = [i for i,x in enumerate(x_3D[:,0,0]) if abs(x-location[0])<tol]
	index_y = [i for i,x in enumerate(y_3D[0,:,0]) if abs(x-location[1])<tol]
	index_z = [i for i,x in enumerate(z_3D[0,0,:]) if abs(x-location[2])<tol]
	index = [0,0,0]
	index[0] = index_x[0]
	index[1] = index_y[0]
	index[2] = index_z[0]
	return index

def get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location,dir):
	tol = 1e-15
	index = [0,0,0]
	# index[0] = [i for i,x in enumerate(x_3D[:,0,0]) if abs(x-location[0])<tol][0]
	# index[1] = [i for i,x in enumerate(y_3D[0,:,0]) if abs(x-location[1])<tol][0]
	# index[2] = [i for i,x in enumerate(z_3D[0,0,:]) if abs(x-location[2])<tol][0]

	index[0] = [abs(x-location[0]) for i,x in enumerate(x_3D[:,0,0])]
	index[1] = [abs(x-location[1]) for i,x in enumerate(y_3D[0,:,0])]
	index[2] = [abs(x-location[2]) for i,x in enumerate(z_3D[0,0,:])]

	index[0] = min(enumerate(index[0]), key=itemgetter(1))[0]
	index[1] = min(enumerate(index[1]), key=itemgetter(1))[0]
	index[2] = min(enumerate(index[2]), key=itemgetter(1))[0]
	return index

def get_center_indexes_old(d_1D):
	index = []
	for d in [0,1,2]:
		index_1D = get_center_indexes_1D(d_1D[:,d])
		index.append(index_1D)
	return index

def get_center_indexes_1D(c):
	tol = 1e-15
	lval = 0.0
	L = abs(c-lval)<tol
	L = [i for i,x in enumerate(L) if x]
	index_1D = L[0]
	return L

def findnth(haystack, needle, n):
	parts = haystack.split(needle, n+1)
	if len(parts)<=n+1:
		return -1
	return len(haystack)-len(parts[-1])-len(needle)

def get_overlapped_data_set(a1,a2,direction): # len(a1)>len(a2)
	b1 = [];b2 = []; c1=[]; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	if direction==1: x=[1,2]
	if direction==2: x=[0,2]
	if direction==3: x=[0,1]
	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				if abs(a1[i,x[0]])<tol and abs(a1[i,x[1]])<tol:
					b1.append(a1[i,3:])
					b2.append(a2[k,3:])
					c1.append(a2[k,0:3]) # x,y,z must be removed later
				k=k+1
	return (np.array(c1),np.array(b1),np.array(b2))

def get_overlapped_data(a1,a2,direction):
	s1 = a1.shape; s2 = a2.shape
	if (s1[0]>s2[0]): (c,interior,total) = get_overlapped_data_set(a1,a2,direction)
	else:			  (c,total,interior) = get_overlapped_data_set(a2,a1,direction)
	return (c,interior,total)

def compute_same_grid(f1,f2,direction,suffix1,suffix2):
	(a_1,h_1) = IO.get_data(f1)
	(a_2,h_2) = IO.get_data(f2)
	h = IO.combine_variables_in_header(h_1,h_2,suffix1,suffix2)
	h = IO.keep_direction_only_in_header(h,direction)
	(c,interior,total) = get_overlapped_data(a_1,a_2,direction)
	s = np.concatenate((c,interior,total),axis=1)
	if direction==1: s = np.delete(s,[1,2],axis=1)
	if direction==2: s = np.delete(s,[0,2],axis=1)
	if direction==3: s = np.delete(s,[0,1],axis=1)
	return (s,h)

def export_difference_same_grid(f1,f2,line_dir,direction,suffix1,suffix2):
	(arr,head) = compute_same_grid(f1,f2,direction,suffix1,suffix2)
	np.savetxt(line_dir, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def compare_PV_RV(root,source,target,v,direction,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	if direction==1: d = 'x'
	if direction==2: d = 'y'
	if direction==3: d = 'z'
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		# print('PVs='+PVs.replace(root,''))
		# print('PVt='+PVt.replace(root,''))
		# print('RVs='+RVs.replace(root,''))
		# print('RVt='+RVt.replace(root,''))
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')
		var_suffix = diff_name+' '
		var_suffix = '' # Depends on needs for Tecplot file...
		diff_name = diff_name+'_'+v+'np'+'_along_'+d+'.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print('diff_file='+diff_dir.replace(root,'')+diff_name)
		export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name,direction,var_suffix+'PV',var_suffix+'RV')

def energy_budget_1D(root,source,target,direction,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	if direction==1: d = 'x'
	if direction==2: d = 'y'
	if direction==3: d = 'z'
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		# print('PVs='+PVs.replace(root,''))
		# print('PVt='+PVt.replace(root,''))
		# print('RVs='+RVs.replace(root,''))
		# print('RVt='+RVt.replace(root,''))
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')
		var_suffix = diff_name+' '
		var_suffix = '' # Depends on needs for Tecplot file...
		diff_name = diff_name+'_'+v+'np'+'_along_'+d+'.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print('diff_file='+diff_dir.replace(root,'')+diff_name)
		export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name,direction,var_suffix+'PV',var_suffix+'RV')

def plot_all_files_PV_RV(root,source,target,x_label,y_label,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		# print('PVs='+PVs.replace(root,''))
		# print('PVt='+PVt.replace(root,''))
		# print('RVs='+RVs.replace(root,''))
		# print('RVt='+RVt.replace(root,''))
		# print('diff_dir='+diff_dir.replace(root,''))
		onlyfiles = IO.get_all_files_in_path(diff_dir)
		onlyfiles = [x for x in onlyfiles if 'along' in x]
		print('\n'.join(onlyfiles))
		ymax = []; xmax = []
		for filename in onlyfiles:
			file = diff_dir+PS+filename
			print('file='+file.replace(root,''))
			(arr,header) = IO.get_data(file)
			(x,y) = IO.get_vec_data_np(arr)
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
		# plt.axis([0, x_max, 0, y_max*1.2])
		# plt.legend(loc='upper left')
		plt.draw()
	plt.show()

def export_line(root,source,target,v,direction,PS):
	if direction==1: d = 'x'
	if direction==2: d = 'y'
	if direction==3: d = 'z'
	t = target
	for s in source:
		# print('PVs='+PVs.replace(root,''))
		# print('PVt='+PVt.replace(root,''))
		# print('RVs='+RVs.replace(root,''))
		# print('RVt='+RVt.replace(root,''))
		filename = IO.get_file_from_path(s)
		ext = IO.get_file_extension(s)
		file_name = IO.get_full_filename_without_extension(s)
		print('filename = '+filename)
		# print('ext = '+ext)
		# print('file_name = '+file_name)
		# diff_name = v+'np'+'_along_'+d+'.dat'
		# suffix = v+'field'+PS+v+'np.dat'

		print('source = '+s.replace(root,''))
		print('target = '+t)
		print('target = '+t.replace(root,''))
		# print('diff_file='+diff_dir.replace(root,'')+diff_name)
		# export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name,direction,var_suffix+'PV',var_suffix+'RV')

def extract_save_line_from_file(root,file_source,file_target,line,PS):
	(arr,head) = extract_line_from_file(file_source,line)
	np.savetxt(file_target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def compute_export_2_dimensionalization(root,file_source,file_target,line,PS):
	(arr,head) = extract_line_from_file(file_source,line)
	i_center = math.floor(arr[:,1].shape[0]/2) # Not strictly correct, need to use get_3D_data_set_shape
	np.seterr(divide='ignore')
	np.seterr(invalid='ignore')
	arr[:,1] = abs(arr[:,1]/arr[i_center,1] - 1.0)
	arr[:,2] = abs(arr[:,2]/arr[i_center,2] - 1.0)
	arr[:,3] = abs(arr[:,3]/arr[i_center,3] - 1.0)
	arr[:,1] = MF.convert_inf_nan_to_zero(arr[:,1])
	arr[:,2] = MF.convert_inf_nan_to_zero(arr[:,2])
	arr[:,3] = MF.convert_inf_nan_to_zero(arr[:,3])
	np.savetxt(file_target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def compute_percent_2_dimensional(f):
	for k in range(1,3):
		i_center = math.floor(f[:,k].shape[0]/2)
		for i in range(0,len(f)):
			if   math.isclose(f[i,k], 0.0, abs_tol=0.00001):
				f[i,k] = 0.0
			else:
				f[i,k] = f[i,k]/f[i_center,k] - 1.0
	return f

def get_SS_value(file_name):
	(arr,header) = IO.get_data(file_name)
	(x,y) = IO.get_vec_data_np(arr)
	return y[-1]

class coordinates:
	hn = []
	hc = []
	dhn = []
	dhc = []
	dAn = []
	dAc = []
	def __init__(self):
		return
