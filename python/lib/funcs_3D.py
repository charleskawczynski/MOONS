import numpy as np
# import pylab
import matplotlib.pyplot as plt
import sys
import file_IO as IO

def get_overlapped_data_set(a1,a2): # len(a1)>len(a2)
	b1 = [];b2 = []; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				b1.append(a1[i,:])
				b2.append(a2[k,:])
				k=k+1
	return (np.array(b1),np.array(b2))

def get_interior_data_set(a1,a2):
	s1 = a1.shape; s2 = a2.shape
	if (s1[0]>s2[0]): (b1,b2) = get_overlapped_data_set(a1,a2)
	else:			  (b2,b1) = get_overlapped_data_set(a2,a1)
	return (b1,b2)

def compare_data_set(interior,total):
	d = total
	d[:,3:] = d[:,3:] - interior[:,3:] # PV - RV
	d[:,3:] = np.abs(d[:,3:])		   # |PV - RV|
	# E = 0.5*d[:,3:]*d[:,3:]		       # Energy error
	E = 0.5*(np.square(d[:,3])+np.square(d[:,4])+np.square(d[:,5]))    # Energy error
	d_amax_abs = np.amax(np.abs(d[:,3:]))
	E_amax_abs = np.amax(np.abs(E))
	return (d,d_amax_abs,E_amax_abs)

def compute_difference_same_grid(f1,f2):
	(a_1,h_1) = IO.get_data(f1)
	(a_2,h_2) = IO.get_data(f2)
	h = [x.replace('PV','').replace('RV','') for x in h_1]
	s1 = a_1.shape; s2 = a_2.shape
	(interior,total) = get_interior_data_set(a_1,a_2)
	(d,a,e) = compare_data_set(interior,total)
	return (d,h,a,e)

def export_difference_same_grid(f1,f2,diff_dir):
	(arr,head,a,e) = compute_difference_same_grid(f1,f2)
	np.savetxt(diff_dir, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file
	return (a,e)

def compare_PV_RV(root,source,target,v,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	PV_RV_abs_error = []; PV_RV_abs_error_name = []
	PV_RV_energy_error = []
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')+'_diff_'+v+'np.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print('diff_file='+diff_dir.replace(root,'')+diff_name)
		(a,e) = export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name)
		PV_RV_abs_error.append(a)
		PV_RV_energy_error.append(e)
		d = diff_dir+diff_name
		name = d.replace(root,'').replace('diff_','').replace('.dat','').replace(' ','')
		name = name.replace('PP','')
		if name.startswith(PS): name = name[1:]
		PV_RV_abs_error_name.append(name)

	for n,e,err in zip(PV_RV_abs_error_name,PV_RV_energy_error,PV_RV_abs_error):
		print('n,d,e='+n+'\t'+str(err)+'\t'+str(e))

def compute_weighted_difference(f1,scale_f1,f2,scale_f2):
	d = f1
	d[:,3:] = scale_f1*d[:,3:] - scale_f2*f2[:,3:]
	E1 = np.amax(0.5*(np.square(f1[:,3])+np.square(f1[:,4])+np.square(f1[:,5])))
	E2 = np.amax(0.5*(np.square(f2[:,3])+np.square(f2[:,4])+np.square(f2[:,5])))
	print('amax(energy(f1)) = '+str(E1))
	print('amax(energy(f2)) = '+str(E2))
	print('scale_f1 = '+str(scale_f1))
	print('scale_f2 = '+str(scale_f2))
	# print('amax(f1) = '+str(amax(f1[:,3:])))
	# print('amax(f2) = '+str(amax(f2[:,3:])))
	return d

def compute_weighted_difference_overlapping_grid(f1,f2,scale_f1,scale_f2):
	(a_1,h_1) = IO.get_data(f1)
	(a_2,h_2) = IO.get_data(f2)
	(interior,total) = get_interior_data_set(a_1,a_2)
	d = compute_weighted_difference(interior,scale_f1,total,scale_f2)
	h = h_1
	return (d,h)

def export_difference_overlapping_grid(f1,f2,target,scale_f1,scale_f2):
	(arr,head) = compute_weighted_difference_overlapping_grid(f1,f2,scale_f1,scale_f2)
	np.savetxt(target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def get_coordinates_above_tol(arr,tol):
	k = 0
	s = arr.shape
	a = arr[:,3]
	m = amax(a)
	L = []
	for i in range(0,s[0]):
		if (a[i]/m>tol):
			L.append(a[k])
			k=k+1
	return L

def append_coordinates_above_tol(L,f,tol):
	(arr,h) = IO.get_data(f)
	return L + get_coordinates_above_tol(arr,tol)
