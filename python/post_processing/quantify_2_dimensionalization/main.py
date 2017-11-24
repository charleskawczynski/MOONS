import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import convert_N_to_t as CNT
import file_IO as IO
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3
import MOONS_directory as MD
import numpy as np

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
print('root = '+root)
print('target = '+target)

Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100,200]
# Re_m = [100]
# Re_m = [0,1,100]
Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_L = [len(str(x)) for x in Re_m]
Re_m_s_LZ = [(max(Re_m_L)-LZ)*'0'+x for LZ,x in zip(Re_m_L,Re_m_s)]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

# source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]
# source_files_selected = [source_files[2]]
source_files_selected = source_files
# source_files_selected = [source_files[1]]

# print(source_files)
for k in source_files: print(k.root.replace(root,''))

print('################### 1D PRIMITIVE VARIABLES PLOTS ############')
for sf,Rm_LZ,Rm in zip(source_files_selected,Re_m_s_LZ,Re_m_s):
	# f1.compute_export_2_dimensionalization(root,sf.Ufield_m,target+'U_1D_diff_Rm'+Rm_LZ+'_dir1.dat',1,PS)
	# f1.compute_export_2_dimensionalization(root,sf.Ufield_m,target+'U_1D_diff_Rm'+Rm_LZ+'_dir2.dat',2,PS)
	# tf = target+'norm_U_1D_diff_Rm'+Rm_LZ+'_dir3.dat'
	# print(tf)
	# f1.compute_export_2_dimensionalization(root,sf.Ufield_m,tf,3,PS)

	tf = target+'u'+PS+'u_'+Rm_LZ+'_dir3.dat'
	print(tf)
	# print(tf.replace(root,''))
	# print(sf.Ufield_m.replace(root,''))
	f3.compute_export_2_dimensionalization_line(root,sf.Ufield_m,tf,3,PS,Rm)

	# tf = target+'abs(u-u_center)'+PS+'norm_U_3D_diff_Rm'+Rm_LZ+'_dir3.dat'
	# print(tf)
	# f3.compute_export_2_dimensionalization_line(root,sf.Ufield_m,tf,3,PS,Rm)

	# tf = target+'norm_U_3D_diff_Ha_Rm'+Rm_LZ+'_dir3.dat'
	# print(tf)
	# f3.compute_export_2_dimensionalization_Hartmann_layers(root,sf.Ufield_m,tf,3,PS,Rm_LZ)


print('\n Finished')

IO.delete_pyc_files()