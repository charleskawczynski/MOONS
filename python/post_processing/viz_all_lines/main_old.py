import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import convert_N_to_t as CNT
# import scipy as sp
import sliding_window as SW
# import scipy.ndimage.morphology
import tecplot_macro_funcs as TMF
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3
import file_IO as IO
import MOONS_directory as MD
import math_funcs as MF
import my_plot as MP
import numpy as np
import dir_funcs as DF
import tecplot

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
desktop_folder = 'C:'+PS+'Users'+PS+'charl'+PS+'Desktop'+PS
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
# target_root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
target_root = 'out'+PS
macro_dir = desktop_folder+'macros'+PS+'lines'+PS

print('root = '+root)

# Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100,200]
# Re_m = [100]
# Re_m = [0,1000]
Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [0,1,100,500,1000]
# Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_L = [len(str(x)) for x in Re_m]
Re_m_s_LZ = [(max(Re_m_L)-LZ)*'0'+x for LZ,x in zip(Re_m_L,Re_m_s)]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]


image_width_png = 600
image_width_eps = 1275
show_windows = False
include_legend = False
xyz = ['x','y','z']
Bxyz = ['B'+x for x in xyz]
Jxyz = ['J'+x for x in xyz]
Uxyz = ['u','v','w']
var_name_all = Bxyz
# var_name_all = Jxyz
# var_name_all = Uxyz
# p_all = [1,2,3]
p_all = [1]
# p_all = [1,2]
# c_all = [1,2,3]
c_all = [3]
# y_vals = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.0,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9]
# y_vals = [-0.8]
y_vals = [0]


for k in source_files: print(k.root.replace(root,''))
print('')

nlevels = 21

def pre_process(root,source_file,target_root,sub_folder,var_name,component,line_dir,target_file_name,y_val,PS):
	d = DF.xyz_given_dir(line_dir)
	target_dir = target_root+'lines'+PS+sub_folder+PS
	IO.directory_tree('',target_dir,PS)
	target_file = target_dir+target_file_name+'.dat'
	print('source_file = '+str(source_file.replace(root,'')))
	print('target_file = '+str(target_file.replace(root,'')))
	print('')
	if (IO.does_file_exist(target_file)):
		(d,h) = IO.get_data(target_file)
	else:
		(d,h) = f1.extract_line_from_file(source_file,[0.0,y_val],line_dir)

	# ----------------------- Start Normalize B-field
	temp = d[:,3:]
	minval = np.min(temp)
	maxval = np.max(temp)
	d[:,3:] = (d[:,3:]-minval)/(maxval-minval)
	minval_str = str(MF.round_sig(minval, 3))
	maxval_str = str(MF.round_sig(maxval, 3))
	print('minval = '+str(minval))
	print('maxval = '+str(maxval))

	if (not IO.does_file_exist(target_file)):
		IO.save_data_to_file(target_file,d,h)

	# print('component   in main = '+str(component))
	# print('line_dir    in main = '+str(line_dir))
	return (target_dir,target_file,minval,maxval,minval_str,maxval_str)

L = TMF.init_macro()
L = TMF.hide_border(L)

folder_suffix_all = ['_from_'+x for x in xyz]
if var_name_all==Uxyz:
	var_name_legend_all = var_name_all
elif var_name_all==Bxyz:
	var_name_legend_all = ['B<sub>'+x+'</sub><sup>1</sup>' for x in xyz]
elif var_name_all==Jxyz:
	var_name_legend_all = ['J<sub>'+x+'</sub>' for x in xyz]
else: raise NameError('Bad var_name_all in main.py')

first_read = True
i=1
i_symbol=1

for y_val in y_vals:
	for p in p_all:
		for c in c_all:
			i_symbol=1
			for Rm_int,Rm,sf in zip(Re_m,Re_m_s_LZ,source_files):
				var_name = var_name_all[c-1]
				sub_folder = var_name+'_vs_'+DF.xyz_given_dir(p)
				sub_folder = var_name+', y='+str(y_val)
				# sub_folder = 'y='+str(y_val)
				if var_name_all==Uxyz:
					f_name = sf.Ufield
					# f_name = sf.Ufield_m
					z_min = -1.0
					x_min =-1
					x_max = 1
				elif var_name_all==Bxyz:
					f_name = sf.Bfield
					# f_name = sf.Bfield_m
					z_min = -1.5
					x_min =-1.5
					x_max = 1.5
				elif var_name_all==Jxyz:
					f_name = sf.Jfield
					# f_name = sf.Jfield_m
					z_min = -1.05
					x_min =-1.05
					x_max = 1.05
				else: raise NameError('Bad var_name_all in main.py')

				target_file_name = 'Rem'+Rm
										 # pre_process(root,source_file,target_root,sub_folder,var_name,component,line_dir,target_file_name,PS)
				# (target_dir,target_file) = pre_process(root,f_name,     target_root,sub_folder,var_name,c,        p,       target_file_name,y_val,PS)
				(target_dir,target_file,minval,maxval,minval_str,maxval_str) = pre_process(root,f_name,     target_root,sub_folder,var_name,c,        p,       target_file_name,y_val,PS)


				# if minval<0:
				# 	num   = '({B} +'+str(abs(minval))+')'
				# 	denom = '('+str(maxval)+' + '+str(abs(minval))+')'
				# else:
				# 	num   = '({B} - '+str(minval)+')'
				# 	denom = '('+str(maxval)+' - '+str(minval)+')'

				# L = TMF.set_equation(L,'{B_x} = '+num.replace('{B}','{B_mirror_3pn_x}')+'/'+denom)
				# L = TMF.set_equation(L,'{B_y} = '+num.replace('{B}','{B_mirror_3pn_y}')+'/'+denom)
				# L = TMF.set_equation(L,'{B_z} = '+num.replace('{B}','{B_mirror_3pn_z}')+'/'+denom)
				# # L = TMF.set_text_box_top_left(L,'min(B),max(B) = '+minval_str+','+maxval_str,25)
				# L = TMF.set_text_box_top_left(L,'|B| = '+maxval_str,25)
				# # ----------------------- End Normalize B-field

				if first_read:
					(L,var_list) = TMF.read_data_set(L,target_file)
					L = TMF.set_plot_type_dimension(L,1)
					first_read = False
				else:
					(L,var_list) = TMF.append_data_set(L,target_file,var_list)

				Rem_removed_leading_zeros = Rm.lstrip('0')
				if Rem_removed_leading_zeros=='': Rem_removed_leading_zeros = '0'
				# Rm_legend = var_name+', Re_m = '+Rm
				Rm_legend = 'Re<sub>m</sub> = '+Rem_removed_leading_zeros

				L = TMF.set_style(L,i,c+1,Rm_legend)
				if p==1: L = TMF.new_line(L,i,1,c+1,i,Rm_legend)
				if p==2: L = TMF.new_line(L,i,5,c+1,i,Rm_legend)
				if p==3: L = TMF.new_line(L,i,6,c+1,i,Rm_legend)
				# L = TMF.set_y_label(L,var_name)
				L = TMF.set_y_label(L,'')
				L = TMF.set_line_props(L,i)
				L = TMF.show_hide_symbols_all(L,True)
				# L = TMF.set_symbol_wrapper(L,i,i_symbol)
				L = TMF.set_symbol_based_on_Rem(L,i,Rm_int)
				L = TMF.hide_legend(L)
				L = TMF.set_text_size(L,5)
				L = TMF.set_axis_fit(L)
				L = TMF.set_x_axis_range(L,[x_min,x_max])
				if p==3: L = TMF.set_x_axis_range(L,[z_min,0]) # for symmetry
				L = TMF.set_label_offset(L,[60,50])
				if var_name_all==Bxyz: tb_position = [0,97]
				elif var_name_all==Jxyz: tb_position = [0,99]
				elif var_name_all==Uxyz: tb_position = [0,99]
				else: tb_position = [0,99]
				L = TMF.set_text_box(L,var_name_legend_all[c-1],tb_position,50)
				i=i+1
				i_symbol=i_symbol+1
			file_name = target_root+'lines'+PS+sub_folder
			print('file_name = '+file_name)
			if not include_legend: L = TMF.save_style(L,file_name+'.sty')
			if not include_legend: L = TMF.save_image(L,file_name)
			if include_legend:
				position = SW.get_empty_window(file_name+'.png',show_windows)
				print('new legend position = '+str(position))
				L = TMF.set_legend_text_size(L,5)
				L = TMF.show_legend(L,[position[0],position[1]])
				L = TMF.save_style(L,file_name+'.sty')
				L = TMF.save_image(L,file_name)
			L = TMF.delete_text_box(L)
			for k in range(1,i):
				L = TMF.set_active_field(L,k,False)

IO.set_file_contents(macro_dir+'save_lines_image.mcr','\n'.join(L))

IO.delete_pyc_files()
print('Done')