import os
import ntpath
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

def get_nearest_line_data_set(d_3D,line):
	k=0; d_1D = [];
	x = []; y = []; c = []
	s = d_3D.shape; tol = 1e-15
	if line==1:
		a1=1; a2 = 2
	if line==2:
		a1=0; a2 = 2
	if line==3:
		a1=0; a2 = 1
	for i in range(0,s[0]):
		if abs(d_3D[i,a1])<tol and abs(d_3D[i,a2])<tol:
			c.append(d_3D[k,0:3])
			d_1D.append( d_3D[k,3:] )
		k=k+1
	s = np.concatenate((c,d_1D),axis=1)
	s = np.delete(s,[a1,a2],axis=1)
	return s

def extract_line_from_file(root,f,line,PS):
	(a,h) = IO.get_data(f)
	a = get_nearest_line_data_set(a,line)
	h = IO.keep_direction_only_in_header(h,line)
	return (a,h)

def extract_save_line_from_file(root,file_source,file_target,plane,PS):
	(arr,head) = extract_line_from_file(root,file_source,plane,PS)
	np.savetxt(file_target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

