import numpy as np
import pylab
import matplotlib.pyplot as plt
import sys
sys.path.insert(0, 'IO')
import file_IO as IO

def get_overlapped_data_set(a1,a2): # len(a1)>len(a2)
	b1 = [];b2 = []; k=0
	s1 = a1.shape; s2 = a2.shape; tol = 1e-15
	for i in range(0,s1[0]):
		if (k<s2[0]):
			if abs(a1[i,0]-a2[k,0])<tol and abs(a1[i,1]-a2[k,1])<tol and abs(a1[i,2]-a2[k,2])<tol:
				if abs(a1[i,1])<tol and abs(a1[i,2])<tol:
					b1.append(a1[i,:])
					b2.append(a2[k,:])
					k=k+1
	return (np.array(b1),np.array(b2))

def get_interior_data_set(a1,a2):
	s1 = a1.shape; s2 = a2.shape
	if (s1[0]>s2[0]): (interior,total) = get_overlapped_data_set(a1,a2)
	else:			  (total,interior) = get_overlapped_data_set(a2,a1)
	return (interior,total)

def compare_data_set(interior,total):
	d = total
	d[:,3:] = d[:,3:] - interior[:,3:] # PV - RV
	d[:,3:] = np.abs(d[:,3:])		   # |PV - RV|
	E = 0.5*d[:,3:]*d[:,3:]		       # Energy error
	d_amax_abs = np.amax(np.abs(d[:,3:]))
	E_amax_abs = np.amax(np.abs(E))
	return (d,d_amax_abs,E_amax_abs)
	
def compute_difference_same_grid(f1,f2):
	(a_1,h_1) = IO.get_data(f1)
	(a_2,h_2) = IO.get_data(f2)
	h = h_1.replace('PV',''); h = h.replace('RV','')
	s1 = a_1.shape; s2 = a_2.shape
	(interior,total) = get_interior_data_set(a_1,a_2)
	(d,a,e) = compare_data_set(interior,total)
	return (d,h,a,e)

def export_difference_same_grid(f1,f2,diff_dir):
	(arr,head,a,e) = compute_difference_same_grid(f1,f2)
	print arr
	np.savetxt(diff_dir, arr, delimiter='	  ', header = head, comments='') # Save file
	return (a,e)

def compare_PV_RV(root,source,target,v,PS):
	PV_s = [k for k in source if 'PV' in k]; RV_s = [k for k in source if 'RV' in k]
	PV_t = [k for k in target if 'PV' in k]; RV_t = [k for k in target if 'RV' in k]
	PV_RV_abs_error = []; PV_RV_abs_error_name = []
	PV_RV_energy_error = []
	for PVs,RVs,PVt,RVt in zip(PV_s,RV_s,PV_t,RV_t):
		(diff_dir,diff_mismatch) = IO.highest_matching_directory(PVt,RVt,PS)
		# print 'PVs='+PVs.replace(root,''); print 'PVt='+PVt.replace(root,'')
		# print 'RVs='+RVs.replace(root,''); print 'RVt='+RVt.replace(root,'')
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')+'_1D_'+v+'np.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print 'diff_dir='+diff_dir.replace(root,'')
		print 'diff_name='+diff_name
		(a,e) = export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name)
		PV_RV_abs_error.append(a)
		PV_RV_energy_error.append(e)
		d = diff_dir+diff_name
		PV_RV_abs_error_name.append(d.replace(root,''))

	for n,e,err in zip(PV_RV_abs_error_name,PV_RV_energy_error,PV_RV_abs_error):
		print 'dir,error,energy_diff='+n,err,e
