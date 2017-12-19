import numpy as np
import math
import math_funcs as MF
import file_IO as IO
import dir_funcs as DF
import funcs_1D as fun1D
import funcs_2D as fun2D
import mesh_class as mesh
import ops_interp_funcs as ops_interp
import ops_aux_funcs as ops_aux

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

# ****************************************************** total domain funcs

def get_CC_squared_from_face_data(field_file,m):
	(arr,head) = IO.get_data(field_file)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,f) = get_3D_indexed_data_SF(arr,s)
	CC = ops_interp.face_2_CC(f,m)
	CC = CC**2.0
	return CC

def compute_scalar_field_L2_norm(field_file,m):
	CC = get_CC_squared_from_face_data(field_file,m)
	integral = ops_aux.compute_dot_product(CC,m.vol)
	integral_sum = np.sum(integral)
	integral_sum = math.sqrt(integral_sum)
	return integral_sum

def compute_vector_field_L2_norm(fx,fy,fz,m):
	CC_x = get_CC_squared_from_face_data(fx,m)
	CC_y = get_CC_squared_from_face_data(fy,m)
	CC_z = get_CC_squared_from_face_data(fz,m)
	CC = CC_x+CC_y+CC_z
	integral = ops_aux.compute_dot_product(CC,m.vol)
	integral_sum = np.sum(integral)
	integral_sum = math.sqrt(integral_sum)
	return integral_sum

# ****************************************************** interior funcs

def get_CC_squared_from_face_data_interior(field_file,m,m_interior):
	(arr,head) = IO.get_data(field_file)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,f) = get_3D_indexed_data_SF(arr,s)
	CC = ops_interp.face_2_CC(f,m)
	CC = CC**2.0
	CC = ops_aux.zero_outside_interior_CC_data(CC,m,m_interior)
	return CC

def compute_scalar_field_L2_norm_interior(field_file,m,m_interior):
	CC = get_CC_squared_from_face_data_interior(field_file,m,m_interior)

	integral = ops_aux.compute_dot_product(CC,m.vol)
	integral_sum = np.sum(integral)
	integral_sum = math.sqrt(integral_sum)
	integral_sum = 0.5*integral_sum
	return integral_sum

def compute_vector_field_L2_norm_interior(fx,fy,fz,m,m_interior):
	CC_x = get_CC_squared_from_face_data_interior(fx,m,m_interior)
	CC_y = get_CC_squared_from_face_data_interior(fy,m,m_interior)
	CC_z = get_CC_squared_from_face_data_interior(fz,m,m_interior)
	CC = CC_x+CC_y+CC_z

	integral = ops_aux.compute_dot_product(CC,m.vol)
	integral_sum = np.sum(integral)
	integral_sum = math.sqrt(integral_sum)
	integral_sum = 0.5*integral_sum
	return integral_sum
# ****************************************************** interior funcs total field

def get_CC_from_face_data_interior(field_file,m,m_interior):
	(arr,head) = IO.get_data(field_file)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,f) = get_3D_indexed_data_SF(arr,s)
	CC = ops_interp.face_2_CC(f,m)
	return CC

def compute_vector_field_L2_norm_interior_total_field(fx,fy,fz,m,m_interior,f0x,f0y,f0z):
	CC1_x = get_CC_from_face_data_interior(fx,m,m_interior)
	CC1_y = get_CC_from_face_data_interior(fy,m,m_interior)
	CC1_z = get_CC_from_face_data_interior(fz,m,m_interior)
	if f0x: CC0_x = get_CC_from_face_data_interior(f0x,m,m_interior)
	else: CC0_x = 0.0
	if f0y: CC0_y = get_CC_from_face_data_interior(f0y,m,m_interior)
	else: CC0_y = 0.0
	if f0z: CC0_z = get_CC_from_face_data_interior(f0z,m,m_interior)
	else: CC0_z = 0.0

	CC_x = (CC1_x+CC0_x)**2.0
	CC_y = (CC1_y+CC0_y)**2.0
	CC_z = (CC1_z+CC0_z)**2.0
	CC = CC_x+CC_y+CC_z
	CC = ops_aux.zero_outside_interior_CC_data(CC,m,m_interior)

	integral = ops_aux.compute_dot_product(CC,m.vol)
	integral_sum = np.sum(integral)
	integral_sum = math.sqrt(integral_sum)
	integral_sum = 0.5*integral_sum
	return integral_sum

# ******************************************************

def compute_2_dimensionalization_iso_surfaces(file_source,plane,location_N,location_C,location_P):
	(arr,head) = IO.get_data(file_source)
	s = IO.get_shape(head)
	np.seterr(divide='ignore')
	np.seterr(invalid='ignore')
	(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D) = get_3D_indexed_data(arr,s)
	N = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_N,plane)
	C = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_C,plane)
	P = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_P,plane)
	s_total = s[0]*s[1]*s[2]
	p = plane-1
	for i in range(0,C[p]):
		(i_select,coeff) = get_Hartmann_layer_index_and_coeff(i,N[p],C[p],P[p])
		(u_3D,v_3D,w_3D) = compute_from_field(u_3D,v_3D,w_3D,i,i_select,coeff,plane)
	for i in reversed(range(C[p],s[0])):
		(i_select,coeff) = get_Hartmann_layer_index_and_coeff(i,N[p],C[p],P[p])
		(u_3D,v_3D,w_3D) = compute_from_field(u_3D,v_3D,w_3D,i,i_select,coeff,plane)
	(u_3D,v_3D,w_3D) = set_to_zero_window(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s,plane,location_N,location_C,location_P)

	(x_1D,y_1D,z_1D,u_1D,v_1D,w_1D) = get_1D_indexed_fields(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s_total)
	arr = np.vstack((x_1D,y_1D,z_1D,u_1D,v_1D,w_1D))
	arr = np.transpose(arr)
	m = mesh.get_mesh(x_3D,y_3D,z_3D,s)
	for k in range(0,6): arr[:,k] = MF.convert_inf_nan_to_zero(arr[:,k])
	return (arr,m,head)

def compute_export_2_dimensionalization_line(root,file_source,file_target,plane,PS,Rem):
	location_N = [ 0.0, 0.0, 0.0]
	location_C = [ 0.0, 0.0, 0.0]
	location_P = [ 0.0, 0.0, 0.0]
	(arr,m,head) = compute_2_dimensionalization_iso_surfaces(file_source,plane,location_N,location_C,location_P)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D) = get_3D_indexed_data(arr,s)

	(arr_x,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.0],3)
	(arr_y,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.0],3)
	(arr_z,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.0],3)
	print('line_location_actual')
	print(line_location_actual)
	arr_x = np.delete(arr_x,[2,3],axis=1)
	arr_y = np.delete(arr_y,[1,3],axis=1)
	arr_z = np.delete(arr_z,[1,2],axis=1)

	# file_path = PS.join(file_target.split(PS)[0:-1])+PS
	# file_name = file_target.split(PS)[-1].replace('.dat','')
	# file_name_x = file_path+'u_'+file_name+'.dat'
	# file_name_y = file_path+'v_'+file_name+'.dat'
	# file_name_z = file_path+'w_'+file_name+'.dat'
	# file_name = file_path+'all_'+file_name+'.dat'
	# head_x = get_1D_header(head,m,plane,Rem)
	# head_y = get_1D_header(head,m,plane,Rem)
	# head_z = get_1D_header(head,m,plane,Rem)
	# np.savetxt(file_name, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file
	# np.savetxt(file_name_x, arr_x, delimiter='	  ', header = ''.join(head_x), comments='') # Save file
	# np.savetxt(file_name_y, arr_y, delimiter='	  ', header = ''.join(head_y), comments='') # Save file
	# np.savetxt(file_name_z, arr_z, delimiter='	  ', header = ''.join(head_z), comments='') # Save file
	save_each_component(file_target,m,head,arr,arr_x,arr_y,arr_z,plane,PS,Rem)

def save_each_component(file_target,m,head,arr,arr_x,arr_y,arr_z,dir,PS,Rem):
	file_path = PS.join(file_target.split(PS)[0:-1])+PS
	file_name = file_target.split(PS)[-1].replace('.dat','')
	file_name_x = file_path+'u_'+file_name+'.dat'
	file_name_y = file_path+'v_'+file_name+'.dat'
	file_name_z = file_path+'w_'+file_name+'.dat'
	# file_name = file_path+'all_'+file_name+'.dat'
	head_x = get_1D_header(head,m,dir,Rem)
	head_y = get_1D_header(head,m,dir,Rem)
	head_z = get_1D_header(head,m,dir,Rem)
	# np.savetxt(file_name, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file
	np.savetxt(file_name_x, arr_x, delimiter='	  ', header = ''.join(head_x), comments='') # Save file
	np.savetxt(file_name_y, arr_y, delimiter='	  ', header = ''.join(head_y), comments='') # Save file
	np.savetxt(file_name_z, arr_z, delimiter='	  ', header = ''.join(head_z), comments='') # Save file

def get_1D_header(head,m,dir,Rem):
	h = [x for x in head]
	Rem_removed_leading_zeros = Rem.lstrip('0')
	if Rem_removed_leading_zeros=='': Rem_removed_leading_zeros = '0'
	h[1] = ' VARIABLES = "'+DF.xyz_given_dir(dir)+'","Re<sub>m</sub> = '+Rem_removed_leading_zeros+'"\n'
	# h[1] = ' VARIABLES = "x","y","z","Re<sub>m</sub> = '+Rem+'"\n'
	h[2] = ' ZONE , T ="1", I ='+str(len(m.c[dir-1].hn))+' DATAPACKING = POINT'
	return h

def get_Hartmann_layer_index_and_coeff(i,i_N,i_C,i_P):
	if i>i_P: i_select=i_P
	elif i<i_N: i_select=i_N
	else: i_select=i_C
	coeff = get_zero_window_coeff(i,i_N,i_C,i_P)
	return (i_select,coeff)

def get_zero_window_coeff(i,i_N,i_C,i_P):
	if i>i_P or i<i_N: coeff=1.0
	else: coeff=0.0
	return coeff

def integrate_plane(X,m,plane,direction,s):
	if direction==1:
		int_X_dA_ijk = [X[plane,j,k]*m.c[0].dAn[j,k] for j in range(0,s[1]) for k in range(0,s[2])]
		int_X_dA = sum(int_X_dA_ijk)
	elif direction==2:
		int_X_dA_ijk = [X[i,plane,k]*m.c[1].dAn[i,k] for i in range(0,s[0]) for k in range(0,s[2])]
		int_X_dA = sum(int_X_dA_ijk)
	elif direction==3:
		int_X_dA_ijk = [X[i,j,plane]*m.c[2].dAn[i,j] for i in range(0,s[0]) for j in range(0,s[1])]
		int_X_dA = sum(int_X_dA_ijk)
	return int_X_dA

def integrate_all_planes(X,m,direction,s):
	int_X_dA = [integrate_plane(X,m,i,direction,s) for i in range(0,s[direction-1])]
	return int_X_dA

def compute_from_field(X,Y,Z,i,i_select,coeff,dir):
	if dir==1: X[i,:,:] = compute_quantity(X[i,:,:],X[i_select,:,:],coeff)
	if dir==1: Y[i,:,:] = compute_quantity(Y[i,:,:],Y[i_select,:,:],coeff)
	if dir==1: Z[i,:,:] = compute_quantity(Z[i,:,:],Z[i_select,:,:],coeff)
	if dir==2: X[:,i,:] = compute_quantity(X[:,i,:],X[:,i_select,:],coeff)
	if dir==2: Y[:,i,:] = compute_quantity(Y[:,i,:],Y[:,i_select,:],coeff)
	if dir==2: Z[:,i,:] = compute_quantity(Z[:,i,:],Z[:,i_select,:],coeff)
	if dir==3: X[:,:,i] = compute_quantity(X[:,:,i],X[:,:,i_select],coeff)
	if dir==3: Y[:,:,i] = compute_quantity(Y[:,:,i],Y[:,:,i_select],coeff)
	if dir==3: Z[:,:,i] = compute_quantity(Z[:,:,i],Z[:,:,i_select],coeff)
	return (X,Y,Z)

def compute_quantity(X,X_sel,coeff):
	add_to = 1.0-coeff
	# return coeff*abs(X/X_sel - 1.0)+add_to*X
	return coeff*abs(X - X_sel)

def compute_export_2_dimensionalization_Hartmann_layers(root,file_source,file_target,plane,PS,Rem):
	location_N = [-0.9,-0.9,-0.9]
	location_C = [ 0.0, 0.0, 0.0]
	location_P = [ 0.9, 0.9, 0.9]
	(arr,m,head) = compute_2_dimensionalization_iso_surfaces(file_source,plane,location_N,location_C,location_P)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D) = get_3D_indexed_data(arr,s)

	(arr_x,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.8],3)
	(arr_y,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.8],3)
	(arr_z,line_location_actual) = fun1D.get_nearest_line_data_set(arr,[0.0,0.8],3)
	print('line_location_actual')
	print(line_location_actual)
	arr_x = np.delete(arr_x,[2,3],axis=1)
	arr_y = np.delete(arr_y,[1,3],axis=1)
	arr_z = np.delete(arr_z,[1,2],axis=1)
	save_each_component(file_target,m,head,arr,arr_x,arr_y,arr_z,plane,PS,Rem)

	# compute_integration = True
	# compute_integration = False
	# if compute_integration:
	# 	temp = integrate_all_planes(u_3D,m,3,s)
	# 	a = np.array(m.c[2].hn)
	# 	b = np.array(temp)
	# 	arr = np.vstack((a,b))
	# 	arr = arr.T
	# 	head[1] = ' VARIABLES = "z","PD_'+Rem+'"\n'
	# 	head[2] = ' ZONE , T ="1", I ='+str(len(m.c[plane-1].hn))+' DATAPACKING = POINT'

	# np.savetxt(file_target, arr, delimiter='	  ', header = ''.join(head), comments='') # Save file

def set_to_zero_window(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s,plane,location_N,location_C,location_P):
	[i_N,j_N,k_N] = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_N,plane)
	[i_C,j_C,k_C] = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_C,plane)
	[i_P,j_P,k_P] = fun1D.get_nearest_1D_indexes_from_coordinates(x_3D,y_3D,z_3D,location_P,plane)
	if plane==1:
		for i in range(0,s[0]):
			coeff = get_zero_window_coeff(i,i_N,i_C,i_P)
			u_3D[i,:,:] = coeff*u_3D[i,:,:]
			v_3D[i,:,:] = coeff*v_3D[i,:,:]
			w_3D[i,:,:] = coeff*w_3D[i,:,:]
	if plane==2:
		for j in range(0,s[1]):
			coeff = get_zero_window_coeff(j,j_N,j_C,j_P)
			u_3D[:,j,:] = coeff*u_3D[:,j,:]
			v_3D[:,j,:] = coeff*v_3D[:,j,:]
			w_3D[:,j,:] = coeff*w_3D[:,j,:]
	if plane==3:
		for k in range(0,s[2]):
			coeff = get_zero_window_coeff(k,k_N,k_C,k_P)
			u_3D[:,:,k] = coeff*u_3D[:,:,k]
			v_3D[:,:,k] = coeff*v_3D[:,:,k]
			w_3D[:,:,k] = coeff*w_3D[:,:,k]
	return (u_3D,v_3D,w_3D)

def get_3D_indexed_data(arr,s):
	x_3D = arr[:,0].reshape(s[0],s[1],s[2],order='F')
	y_3D = arr[:,1].reshape(s[0],s[1],s[2],order='F')
	z_3D = arr[:,2].reshape(s[0],s[1],s[2],order='F')
	u_3D = arr[:,3].reshape(s[0],s[1],s[2],order='F')
	v_3D = arr[:,4].reshape(s[0],s[1],s[2],order='F')
	w_3D = arr[:,5].reshape(s[0],s[1],s[2],order='F')
	return (x_3D,y_3D,z_3D,u_3D,v_3D,w_3D)

def get_3D_indexed_data_SF(arr,s):
	x_3D = arr[:,0].reshape(s[0],s[1],s[2],order='F')
	y_3D = arr[:,1].reshape(s[0],s[1],s[2],order='F')
	z_3D = arr[:,2].reshape(s[0],s[1],s[2],order='F')
	f    = arr[:,3].reshape(s[0],s[1],s[2],order='F')
	return (x_3D,y_3D,z_3D,f)

def get_1D_indexed_fields(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s_total):
	x_1D = x_3D.reshape(s_total,order='F')
	y_1D = y_3D.reshape(s_total,order='F')
	z_1D = z_3D.reshape(s_total,order='F')
	u_1D = u_3D.reshape(s_total,order='F')
	v_1D = v_3D.reshape(s_total,order='F')
	w_1D = w_3D.reshape(s_total,order='F')
	return (x_1D,y_1D,z_1D,u_1D,v_1D,w_1D)

def get_1D_indexed_data(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s_total):
	(x_1D,y_1D,z_1D,u_1D,v_1D,w_1D) = get_1D_indexed_fields(x_3D,y_3D,z_3D,u_3D,v_3D,w_3D,s_total)
	arr = np.vstack((x_1D,y_1D,z_1D,u_1D,v_1D,w_1D))
	arr = np.transpose(arr)
	return arr
