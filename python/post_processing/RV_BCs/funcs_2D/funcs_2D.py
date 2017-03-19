import numpy as np
import pylab
import matplotlib.pyplot as plt
import sys
sys.path.insert(0, 'IO')
import file_IO as IO

def get_plane_data_set(d_3D,plane):
	k=0; d_2D = []; 
	x = []; y = []; c = []
	s = d_3D.shape; tol = 1e-15
	if plane==1: p=0
	if plane==2: p=1
	if plane==3: p=2
	for i in range(0,s[0]):
		if abs(d_3D[i,p])<tol:
			c.append(d_3D[k,0:3])
			d_2D.append( d_3D[k,3:] )
		k=k+1
	s = np.concatenate((c,d_2D),axis=1)
	s = np.delete(s,p,axis=1)
	return s

def extract_plane_from_file(root,f,plane,PS):
	(a,h) = IO.get_data(f)
	a = get_plane_data_set(a,plane)
	h = IO.neglect_direction_in_header(h,plane)
	return (a,h)

def extract_save_plane_from_file(root,fs,ft,plane,PS):
	(arr,head) = extract_plane_from_file(root,fs,plane,PS)
	np.savetxt(ft, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

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
		# print 'fs='+file_name_s.replace(root,'')
		print 'ft='+file_name_t.replace(root,'')
		extract_save_plane_from_file(root,file_name_s,file_name_t,plane,PS)


