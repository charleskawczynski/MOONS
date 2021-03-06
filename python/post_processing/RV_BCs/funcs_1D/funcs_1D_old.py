import numpy as np
import pylab
import matplotlib.pyplot as plt
import sys
sys.path.insert(0, 'IO')
import file_IO as IO

def get_interior_data_set_sorted(a1,a2): # len(a1)>len(a2)
	interior = []; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				interior.append(a1[i,:]); k=k+1
	return np.array(interior)

def get_interior_data_set_sorted_1D(a1,a2,direction): # len(a1)>len(a2)
	interior = []; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	big_tol = 1e-1
	if direction==1: x=[1,2]
	elif direction==2: x=[0,2]
	elif direction==3: x=[0,1]
	else: sys.error('Error: direction must = 1,2,3 in get_interior_1D_data_set_sorted')

	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				print 'point='+str(a2[k,0])+','+str(a2[k,1])+','+str(a2[k,2])
				# if abs(a1[i,0])<tol:
					interior.append(a1[i,:]); k=k+1
					# interior.append(np.insert(a1[i,3:],0,a1[i,direction-1])); k=k+1
	return np.array(interior)

def get_interior_data_set_1D(a1,a2,direction):
	s1 = a1.shape; s2 = a2.shape
	if (s1[0]>s2[0]): interior = get_interior_data_set_sorted_1D(a1,a2,direction); total = a2
	else:			  interior = get_interior_data_set_sorted_1D(a2,a1,direction); total = a1
	return (interior,total)

def compare_data_set(interior,total):
	d = total
	d[:,3:] = d[:,3:] - interior[:,3:] # PV - RV
	d[:,3:] = np.abs(d[:,3:])		   # |PV - RV|
	E = 0.5*d[:,3:]*d[:,3:]		       # Energy error
	d_amax_abs = np.amax(np.abs(d[:,3:]))
	E_amax_abs = np.amax(np.abs(E))
	return (d,d_amax_abs,E_amax_abs)

def compute_1D_same_grid(f1,f2,direction):
	(a_1,h_1) = IO.get_data(f1)
	(a_2,h_2) = IO.get_data(f2)
	h = h_1.replace('PV',''); h = h.replace('RV','')
	s1 = a_1.shape; s2 = a_2.shape
	(interior,total) = get_interior_data_set_1D(a_1,a_2,direction)
	(interior,total) = get_interior_data_set_1D(a_1,a_2,direction)
	(d,a,e) = compare_data_set(interior,total)
	print 'interior='
	print interior
	print 'total='
	print total
	d = np.array([interior,total])
	return (d,h)

def export_difference_same_grid_1D(f1,f2,line_dir,direction):
	(arr,head) = compute_1D_same_grid(f1,f2,direction)
	np.savetxt(line_dir, arr, delimiter='	  ', header = head, comments='') # Save file

def compare_PV_RV_1D(root,source,target,v,direction,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		# print 'PVs='+PVs.replace(root,''); print 'PVt='+PVt.replace(root,'')
		# print 'RVs='+RVs.replace(root,''); print 'RVt='+RVt.replace(root,'')
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')+'_1D_'+v+'np.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print 'diff_dir='+diff_dir.replace(root,'')
		print 'diff_name='+diff_name
		export_difference_same_grid_1D(PVs+suffix,RVs+suffix,diff_dir+diff_name,direction)

