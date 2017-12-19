import numpy as np
import pylab
import matplotlib.pyplot as plt
import sys
import file_IO as IO

def get_plane_data_set(d_3D,plane):
	p=plane-1
	tol = 1e-15
	pval = 0.0
	L = abs(d_3D[:,p]-pval)<tol
	d_2D_all = d_3D[L,:]
	c = d_2D_all[:,0:3]
	d_2D = d_2D_all[:,3:]
	s = np.concatenate((c,d_2D),axis=1)
	s = np.delete(s,p,axis=1)
	return s

def extract_plane_from_file(f,plane):
	(a_raw,h) = IO.get_data(f)
	a = get_plane_data_set(a_raw,plane)
	h = IO.neglect_direction_in_header(h,plane)
	return (a,h)

def extract_save_plane_from_file(root,file_source,file_target,plane,PS):
	(arr,head) = extract_plane_from_file(root,file_source,plane,PS)
	np.savetxt(file_target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def extract_plane(root,source,target,v,plane,PS):
	f_s = [k for k in source]
	f_t = [k for k in target]
	if plane==1: d = 'x'
	if plane==2: d = 'y'
	if plane==3: d = 'z'
	ext = '.dat'
	for fs,ft in zip(f_s,f_t):
		file_name_s = fs+v+'field'+PS+v+'np'+ext
		file_name_t = ft+v+'field'+PS+v+'np_'+d+'_plane'+ext
		# print('fs='+file_name_s.replace(root,''))
		print('ft='+file_name_t.replace(root,''))
		# extract_save_plane_from_file(root,file_name_s,file_name_t,plane,PS)

def compute_export_2_dimensionalization_plane(root,file_source,file_target,plane,PS):
	(arr,head) = extract_plane_from_file(file_source,plane)
	# i_center = math.ceil(arr[:,1].shape[0]/2)
	print(arr.shape)
	print(arr.shape[0])
	# print(arr)
	# s = IO.get_shape(head)
	# U = arr.reshape()
	# U0 = arr.reshape()
	# print(s)
	# for x in range(0,s[0]):
	# 	for y in range(0,s[1]):
	# 		print(arr[x,y,1])
	return f


