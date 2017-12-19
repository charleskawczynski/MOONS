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
root = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
macro_dir = 'C:'+PS+'Users'+PS+'charl'+PS+'Desktop'+PS+'macros'+PS+'save_image'+PS
print('root = '+root)

Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100,200]
# Re_m = [100]
Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

test_file = 'C:'+PS+'Users'+PS+'charl'+PS+'Documents'+PS+'MOONS'+PS+'SIMS'+PS+'test'+PS+'test.dat'

symbol_markers = ['r-','bo']
# source_files = [source_files[1], source_files[2]]
source_files = [source_files[10]]

for k in source_files: print(k.root.replace(root,''))
print('')

nlevels = 21

DP = [MP.my_plot() for k in source_files]

def pre_process(source_file,var_name,component,line_dir,target_file_name,PS):
	d = DF.xyz_given_dir(line_dir)
	a = DF.adj_xyz_given_dir(line_dir)
	a_i = DF.adj_given_dir(line_dir)
	target_dir = 'out'+PS+var_name+'_vs_'+d+PS
	IO.directory_tree('',target_dir,PS)
	dp.set_meta(target_dir,target_file_name,d,var_name)
	dp.set_font_size(15)
	target_file = target_dir+target_file_name+'.dat'
	if (IO.does_file_exist(target_file)): (d,h) = IO.get_data(target_file)
	else: (d,h) = f1.extract_line_from_file(source_file,line_dir)
	dp.set_data(d,h)
	if (not IO.does_file_exist(target_file)): dp.write_data_to_file()
	dp.set_active_directions()
	print('component in main = '+str(component))
	print('line_dir  in main = '+str(line_dir))
	dp.process_data(component)
	dp.compute_range()
	return target_dir

def process_dir(var_name,line_dir,ID,PS):
	d = DF.xyz_given_dir(line_dir)
	a = DF.adj_xyz_given_dir(line_dir)
	a_i = DF.adj_given_dir(line_dir)
	target_dir = 'out'+PS+var_name+'_vs_'+d+PS
	target_file_name = 'Rm'+str(ID)
	IO.directory_tree('',target_dir,PS)
	target_file = target_dir+target_file_name+'.dat'
	if ID==10:
		target_file_name_easy_import = 'Rm'+str(ID)
	else:
		target_file_name_easy_import = 'Rm0'+str(ID)
	target_file_easy_import = target_dir+target_file_name_easy_import+'.dat'
	# IO.rename_file(target_file,target_file_easy_import)
	if IO.does_file_exist(target_file_easy_import):
		if IO.does_file_exist(target_file):
			os.remove(target_file)

def compute_global_range(DP):
	r_buffer = 1.1
	range_all = [(dp.xRange,dp.yRange,dp.zRange) for dp in DP]
	xRange_global = [np.min([k.xRange[0] for k in DP])*r_buffer, np.max([r.xRange[1] for r in DP])*r_buffer]
	yRange_global = [np.min([k.yRange[0] for k in DP])*r_buffer, np.max([r.yRange[1] for r in DP])*r_buffer]
	zRange_global = [np.min([k.zRange[0] for k in DP])*r_buffer, np.max([r.zRange[1] for r in DP])*r_buffer]
	xRange_global = MF.round_range(xRange_global)
	yRange_global = MF.round_range(yRange_global)
	zRange_global = MF.round_range(zRange_global)
	range_global = [xRange_global,yRange_global,zRange_global]
	return range_global

# p=2
# for p in [1,2,3]:
# 	for c in [1,2,3]:
# 		for Rm,sf in zip(Re_m_s_name,source_files): process_dir(DF.uvw_along(c),p,Rm,PS)
# 		for Rm,sf in zip(Re_m_s_name,source_files): process_dir(DF.Bxyz_along(c),p,Rm,PS)
# 		for Rm,sf in zip(Re_m_s_name,source_files): process_dir(DF.Jxyz_along(c),p,Rm,PS)

# U
for p in [1,2,3]:
	for c in [1,2,3]:
		for Rm,sf in zip(Re_m_s_name,source_files): target_dir = pre_process(sf.Ufield_m,DF.uvw_given_dir(c),c,p,Rm,PS)
		range_global = compute_global_range(DP)
		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
		FIGS = []
		for dp in DP: FIGS.append(dp.plot())
		f1.combine_plots(target_dir,FIGS,symbol_markers)
		for dp in DP: dp.close_plots()

xRange = [-2,2]
# B
for p in [1,2,3]:
	for c in [1,2,3]:
		for Rm,sf in zip(Re_m_s_name,source_files): target_dir = pre_process(sf.Bfield_m,DF.Bxyz_given_dir(c),c,p,Rm,PS)
		range_global = compute_global_range(DP)
		range_global[0] = xRange
		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
		FIGS = []
		for dp in DP: FIGS.append(dp.plot())
		f1.combine_plots(target_dir,FIGS,symbol_markers)
		for dp in DP: dp.close_plots()

xRange = [-1.05,1.05]
# J
for p in [1,2,3]:
	for c in [1,2,3]:
		for Rm,sf in zip(Re_m_s_name,source_files): target_dir = pre_process(sf.Jfield_m,DF.Jxyz_given_dir(c),c,p,Rm,PS)
		range_global = compute_global_range(DP)
		range_global[0] = xRange
		for dp,sf in zip(DP,source_files): dp.set_range(range_global,nlevels)
		FIGS = []
		for dp in DP: FIGS.append(dp.plot())
		f1.combine_plots(target_dir,FIGS,symbol_markers)
		for dp in DP: dp.close_plots()


IO.delete_pyc_files()
print('Done')