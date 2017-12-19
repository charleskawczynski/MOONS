import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import convert_N_to_t as CNT
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3
import file_IO as IO
import MOONS_directory as MD
import math_funcs as MF
import my_plot as MP
import numpy as np
import dir_funcs as DF

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
# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [0,1,100,500,1000]
Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_L = [len(str(x)) for x in Re_m]
Re_m_s_LZ = [(max(Re_m_L)-LZ)*'0'+x for LZ,x in zip(Re_m_L,Re_m_s)]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

for k in source_files: print(k.root.replace(root,''))
print('')

nlevels = 21

DP = [MP.my_plot() for k in source_files]

def pre_process(file_source,var_name,component,plane_dir,ID,PS):
	d = DF.xyz_given_dir(plane_dir)
	a = DF.adj_xyz_given_dir(plane_dir)
	a_i = DF.adj_xyz_given_dir_ind(plane_dir)
	target_dir = var_name+'_from_'+d
	file_target = target_dir+PS+'Rm'+str(ID)
	IO.directory_tree('',target_dir,PS)
	dp.set_meta(file_target,a[1],a[0],var_name)
	dp.set_font_size(15)
	(d,h) = f2.extract_plane_from_file(file_source,plane_dir)
	dp.set_data(d,h)
	dp.set_active_directions()
	dp.process_data([1,0,1+component])
	dp.compute_range()

def compute_global_range(DP):
	r_buffer = 1.1
	range_all = [(dp.xRange,dp.yRange,dp.zRange) for dp in DP]
	xRange_global = [np.min([k.xRange[0] for k in DP])*r_buffer, np.max([r.xRange[1] for r in DP])*r_buffer]
	yRange_global = [np.min([k.yRange[0] for k in DP])*r_buffer, np.max([r.yRange[1] for r in DP])*r_buffer]
	zRange_global = [np.min([k.zRange[0] for k in DP])*r_buffer, np.max([r.zRange[1] for r in DP])*r_buffer]
	try: xRange_global = [MF.round_sig(k,2) for k in xRange_global]
	except: xRange_global = [-1,1]
	try: yRange_global = [MF.round_sig(k,2) for k in yRange_global]
	except: yRange_global = [-1,1]
	try: zRange_global = [MF.round_sig(k,2) for k in zRange_global]
	except: zRange_global = [-1,1]
	range_global = [xRange_global,yRange_global,zRange_global]
	return range_global

# U
# c = 1
# p = 3
# for p in range(1,4):
# 	for c in range(1,4):
# 		for dp,sf in zip(DP,source_files): pre_process(sf.Ufield_m,DF.uvw_given_dir(c),c,p,sf.ID,PS)
# 		range_global = compute_global_range(DP)
# 		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
# 		for dp in DP: dp.plot()

xRange = [-2,2]
yRange = [-2,2]
# B
for p in range(1,4):
	for c in range(1,4):
		for dp,sf in zip(DP,source_files): pre_process(sf.Bfield_m,DF.Bxyz_given_dir(c),c,p,sf.ID,PS)
		range_global = compute_global_range(DP)
		range_global[0] = xRange
		range_global[1] = yRange
		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
		for dp in DP: dp.plot()

# xRange = [-1.05,1.05]
# yRange = [-1.05,1]
# # J
# for p in range(1,4):
# 	for c in range(1,4):
# 		for dp,sf in zip(DP,source_files): pre_process(sf.Jfield_m,DF.Jxyz_given_dir(c),c,p,sf.ID,PS)
# 		range_global = compute_global_range(DP)
# 		range_global[0] = xRange
# 		range_global[1] = yRange
# 		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
# 		for dp in DP: dp.plot()


IO.delete_pyc_files()
print('Done')