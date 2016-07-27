import numpy as np
import pylab
import matplotlib.pyplot as plt
import sys
sys.path.insert(0, 'IO')
import file_IO as IO

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
		# print 'PVs='+PVs.replace(root,''); print 'PVt='+PVt.replace(root,'')
		# print 'RVs='+RVs.replace(root,''); print 'RVt='+RVt.replace(root,'')
		diff_name = diff_mismatch.replace(' ','').replace('RV','').replace('PV','')+'_1D_'+d+'_'+v+'np.dat'
		suffix = v+'field'+PS+v+'np.dat'
		print 'diff_dir='+diff_dir.replace(root,'')
		print 'diff_name='+diff_name
		export_difference_same_grid(PVs+suffix,RVs+suffix,diff_dir+diff_name,direction,'PV','RV')

