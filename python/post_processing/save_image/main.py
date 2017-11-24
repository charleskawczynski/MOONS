import sys
import os
clear = lambda: os.system('cls')
clear()
sys.path.insert(0, '../lib/')
import numpy as np
import dir_funcs as DF
import math_funcs as MF
import save_image as SI
import tecplot_macro_funcs as TMF
import MOONS_directory as MD
import sys
import file_IO as IO

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
desktop_folder = 'C:'+PS+'Users'+PS+'charl'+PS+'Desktop'+PS
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target_root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
# target_root = 'out'+PS
macro_dir = desktop_folder+'macros'+PS+'planes'+PS

print('root = '+root)

# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
Re_m = [0,1,100,500,1000]
# Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_L = [len(str(x)) for x in Re_m]
Re_m_s_LZ = [(max(Re_m_L)-LZ)*'0'+x for LZ,x in zip(Re_m_L,Re_m_s)]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

# print(source_files)
for k in source_files: print(k.root.replace(root,''))
# for k in source_files: print(k.root)

# for sf in source_files:
# 	print(sf.Ufield_m.replace(root,''))
# 	print(sf.Bfield_m.replace(root,''))

image_width_png = 600
image_width_eps = 1275
N_approx_contour_levels = 25

# sub_folders_root = [target+x for x in sub_folders]
field_num = 1 # FIXED: needs deactivation for contour levels, and activation for edge!

var_name_U = ['u','v','w']
var_name_B = ['Bx','By','Bz']
var_name_J = ['Jx','Jy','Jz']
var_name_XYZ = ['x','y','z']
planes_all = [1,2,3]
planes_all = [1]
components_all = [1,2,3]
# components_all = [1]

# var_name_all = var_name_U
var_name_all = var_name_B
# var_name_all = var_name_J

folder_suffix_all = ['_from_'+x for x in var_name_XYZ]
if var_name_all==var_name_U:
	var_name_legend_all = var_name_all
elif var_name_all==var_name_B:
	var_name_legend_all = ['B<sub>'+x+'</sub><sup>1</sup>' for x in var_name_XYZ]
	var_name_legend_all = ['<greek>b</greek><sub>'+x+'</sub><sup>1</sup>' for x in var_name_XYZ]
else: raise NameError('Bad var_name_all in main.py')

L = TMF.init_macro()
L = TMF.hide_border(L)
for file_name,sf in zip(Re_m_s_name,source_files):
	print(sf.Ufield_m.replace(root,''))
	if var_name_all==var_name_U:
		(L,var_list) = TMF.read_data_set(L,sf.Ufield_m)
		x_range = [-1.05,1.05]
		y_range = [-1.05,1.05]
		n_data_sets = 1
	elif var_name_all==var_name_B:
		(L,var_list) = TMF.read_data_set(L,sf.Bfield_m)

		(L,var_list) = TMF.append_data_set(L,sf.mesh_mom_m,var_list)
		(L,var_list) = TMF.append_data_set(L,sf.mesh_sig_m,var_list)
		x_range = [-1.5,1.5]
		y_range = [-1.5,1.5]
		n_data_sets = 3

		# ----------------------- Start Normalize B-field
		(arr,H) = IO.get_data(sf.Bfield_m)
		temp = arr[:,3:]
		minval = np.min(temp)
		maxval = np.max(temp)
		minval_str = str(MF.round_sig(minval, 3))
		maxval_str = str(MF.round_sig(maxval, 3))
		print('minval = '+str(minval))
		print('maxval = '+str(maxval))
		if minval<0:
			num   = '({B} +'+str(abs(minval))+')'
			denom = '('+str(maxval)+' + '+str(abs(minval))+')'
		else:
			num   = '({B} - '+str(minval)+')'
			denom = '('+str(maxval)+' - '+str(minval)+')'
		L = TMF.set_equation(L,'{B_x} = '+num.replace('{B}','{B_mirror_3pn_x}')+'/'+denom)
		L = TMF.set_equation(L,'{B_y} = '+num.replace('{B}','{B_mirror_3pn_y}')+'/'+denom)
		L = TMF.set_equation(L,'{B_z} = '+num.replace('{B}','{B_mirror_3pn_z}')+'/'+denom)
		# L = TMF.set_text_box_top_left(L,'min(B),max(B) = '+minval_str+','+maxval_str,25)
		L = TMF.set_text_box_top_left(L,'|B| = '+maxval_str,25)
		# ----------------------- End Normalize B-field

	else: raise NameError('Bad var_name_all in main.py')
	# for plane in [1]:
	for plane in planes_all:
		# for component in [1]:
		for component in components_all:
			var_name_legend = var_name_legend_all[component-1]
			var_name_folder = var_name_all[component-1]
			sub_folder = target_root+var_name_folder+folder_suffix_all[plane-1]
			IO.make_path(sub_folder)
			L = SI.save_new_image(L,root,macro_dir,sub_folder,
				# file_name,var_name_legend,component+3,image_width_png,
				file_name,var_name_legend,component+8,image_width_png, # Needed when additional vars defined
				image_width_eps,field_num,N_approx_contour_levels,
				plane,x_range,y_range,n_data_sets,PS)

IO.set_file_contents(macro_dir+'save_image.mcr','\n'.join(L))

print('\n Finished')

IO.delete_pyc_files()