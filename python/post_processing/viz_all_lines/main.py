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
import directory_tree as DT
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
this_path = os.path.dirname(os.path.realpath(__file__))+PS
target_root = this_path+'out'+PS
macro_dir = desktop_folder+'macros'+PS+'lines'+PS

print('root = '+root)

# Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100,200]
# Re_m = [100]
# Re_m = [0,1000]
# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
Re_m = [0,1,100,500,1000]
# Re_m = [100,1000]
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
p_all = [3]
# p_all = [1,2]
# c_all = [1,2,3]
c_all = [1]
# y_vals = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.0,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7,-0.8,-0.9]
# y_vals = [-0.8]
y_vals = [0]
Normalize = True
prep = False

if var_name_all==Uxyz:
	var_name_legend_all = var_name_all
elif var_name_all==Jxyz:
	var_name_legend_all = ['J<sub>'+x+'</sub>' for x in xyz]
elif var_name_all==Bxyz:
	var_name_legend_all = ['B<sub>'+x+'</sub><sup>1</sup>' for x in xyz]
	var_name_legend_all = ['<greek>b</greek><sub>'+x+'</sub><sup>1</sup>' for x in xyz]
else: raise NameError('Bad var_name_all in main.py')

for k in source_files: print(k.root.replace(root,''))
print('')

nlevels = 21

def pre_process(source_file,target_file,line_dir,y_val,PS):
	d = DF.xyz_given_dir(line_dir)

	min_max_file = target_file.replace('.dat','_min_max.dat')

	IO.print_local_dir(target_file,'target_file',5)
	IO.print_local_dir(source_file,'source_file',8)
	IO.print_local_dir(min_max_file,'min_max_file',5)
	print('')

	if (IO.does_file_exist(target_file)):
		(d,h) = IO.get_data(target_file)
		(d_temp,temp) = IO.get_data(min_max_file,0)
		minval = d_temp[0]
		maxval = d_temp[1]
	else:
		(d,h) = f1.extract_line_from_file(source_file,[0.0,y_val],line_dir)
		temp = d[:,3:]
		minval = np.min(temp)
		maxval = np.max(temp)
		IO.save_data_to_file(min_max_file,(minval,maxval),'')
		IO.save_data_to_file(target_file,d,h)

	minval_str = str(MF.round_sig(minval, 3))
	maxval_str = str(MF.round_sig(maxval, 3))
	return (minval,maxval,minval_str,maxval_str)

L = TMF.init_macro()
L = TMF.hide_border(L)

first_read = True
i=1

D_T = DT.directory_tree()
for y_val in y_vals:
	for p in p_all:
		for c in c_all:
			for Rm_int,Rm,sf in zip(Re_m,Re_m_s_LZ,source_files):
				d = (y_val,p,c,Rm,sf)
				D_T.set_param_set(d,'')

case_IDs = [key for key in D_T.param_set]
print(case_IDs)
# raise NameError('Done in main.py')
var_name = dict()
target_file = dict()
target_image_file_name = dict()
folders = dict()
x_min = dict()
x_max = dict()
z_min = dict()
Rm_legend = dict()
f_name = dict()
sub_folder = dict()
target_dir = dict()
minval = dict()
maxval = dict()
minval_str = dict()
maxval_str = dict()
num = dict()
denom = dict()


for key in case_IDs:
	(y_val,p,c,Rm,sf) = key

	if var_name_all==Uxyz:
		f_name[key] = sf.Ufield
		# f_name[key] = sf.Ufield_m
		z_min[key] = -1.0
		x_min[key] =-1
		x_max[key] = 1
	elif var_name_all==Bxyz:
		f_name[key] = sf.Bfield
		# f_name[key] = sf.Bfield_m
		z_min[key] = -1.5
		x_min[key] =-1.5
		x_max[key] = 1.5
	elif var_name_all==Jxyz:
		f_name[key] = sf.Jfield
		# f_name[key] = sf.Jfield_m
		z_min[key] = -1.05
		x_min[key] =-1.05
		x_max[key] = 1.05
	else: raise NameError('Bad var_name_all in main.py')

	var_name[key] = var_name_all[c-1]

	sub_folder[key] = var_name[key]+'_vs_'+DF.xyz_given_dir(p)
	# sub_folder[key] = var_name+', y='+str(y_val)
	# sub_folder[key] = 'y='+str(y_val)
	target_dir[key] = target_root+'lines'+PS+sub_folder[key]+PS
	IO.directory_tree('',target_dir[key],PS)
	target_file[key] = target_dir[key]+'Rem'+Rm+'.dat'

	(minval[key],maxval[key],minval_str[key],maxval_str[key]) = pre_process(f_name[key]     ,target_file[key],p       ,y_val,PS)

	Rem_removed_leading_zeros = Rm.lstrip('0')
	if Rem_removed_leading_zeros=='': Rem_removed_leading_zeros = '0'
	# Rm_legend[key] = var_name[key]+', Re_m = '+Rm
	Rm_legend[key] = 'Re<sub>m</sub> = '+Rem_removed_leading_zeros
	Rm_legend[key] = Rm_legend[key]+', '+'|B| = '+maxval_str[key]

	if minval[key]<0:
		num[key]   = '({B} +'+str(abs(minval[key]))+')'
		denom[key] = '('+str(maxval[key])+' + '+str(abs(minval[key]))+')'
	else:
		num[key]   = '({B} - '+str(minval[key])+')'
		denom[key] = '('+str(maxval[key])+' - '+str(minval[key])+')'


	if IO.does_file_exist(target_file[key]):
		if first_read:
			(L,var_list) = TMF.read_data_set(L,target_file[key])
			L = TMF.set_plot_type_dimension(L,1)
			first_read = False
		else:
			(L,var_list) = TMF.append_data_set(L,target_file[key],var_list)

	target_image_file_name[key] = target_root+'lines'+PS+sub_folder[key]
	IO.print_local_dir(target_image_file_name[key],'target_image_file_name')


L = TMF.delete_map(L,1)
L = TMF.delete_map(L,1)
L = TMF.delete_map(L,1)
y_axis_indexes_buffer = 1+3
# print('y_axis_indexes_buffer = '+str(y_axis_indexes_buffer))

zone_num = 0
field_num = 0
for key in case_IDs:
	zone_num = zone_num+1
	field_num = field_num+1
	(y_val,p,c,Rm,sf) = key

	# L = TMF.set_equation(L,'{B_x'+str(Rm)+'} = '+num[key].replace('{B}','{Bpn_x}')+'/'+denom[key])
	# L = TMF.set_equation(L,'{B_y'+str(Rm)+'} = '+num[key].replace('{B}','{Bpn_y}')+'/'+denom[key])
	L = TMF.set_equation(L,'{B_z'+str(Rm)+'} = '+num[key].replace('{B}','{Bpn_z}')+'/'+denom[key])

	if p==1: L = TMF.new_line(L,field_num,1,c+1,zone_num,Rm_legend[key])
	if p==2: L = TMF.new_line(L,field_num,1,c+1,zone_num,Rm_legend[key])
	if p==3: L = TMF.new_line(L,field_num,1,c+1,zone_num,Rm_legend[key])
	L = TMF.set_y_axis_var(L,zone_num,y_axis_indexes_buffer+zone_num)

	L = TMF.set_y_label(L,'')
	L = TMF.set_line_props(L,i)
	L = TMF.show_hide_symbols_all(L,True)
	L = TMF.set_symbol_based_on_Rem(L,i,Rm_int)
	L = TMF.hide_legend(L)
	L = TMF.set_text_size(L,5)
	L = TMF.set_axis_fit(L)
	L = TMF.set_x_axis_range(L,[x_min[key],x_max[key]])

	if p==3: L = TMF.set_x_axis_range(L,[z_min[key],0]) # for symmetry
	L = TMF.set_label_offset(L,[60,50])
	if var_name_all==Bxyz: tb_position = [0,97]
	elif var_name_all==Jxyz: tb_position = [0,99]
	elif var_name_all==Uxyz: tb_position = [0,99]
	else: tb_position = [0,99]
	L = TMF.set_text_box(L,var_name_legend_all[c-1],tb_position,50)

	position = SW.get_empty_window(target_image_file_name[key]+'.png',show_windows)
	print('new legend position = '+str(position))
	L = TMF.set_legend_text_size(L,5)
	L = TMF.show_legend(L,[position[0],position[1]])
	L = TMF.save_image(L,target_image_file_name[key])

if False:
	for key in case_IDs:
		(y_val,p,c,Rm) = key

		i=i+1
		print('------------------------------------------------ HERE')

	if not include_legend: L = TMF.save_image(L,target_image_file_name[key])

	if include_legend:

	L = TMF.delete_text_box(L)
	for k in range(1,i):
		L = TMF.set_active_field(L,k,False)


IO.set_file_contents(macro_dir+'save_lines_image.mcr','\n'.join(L))

IO.delete_pyc_files()
print('Done')