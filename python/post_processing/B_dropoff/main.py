# from scipy import stats
import sys
import os
import matplotlib.pyplot as plt
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
print('root = '+root)

LDC_path = PS+'out'+PS+'LDC'
R = range(0,10+1)
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in R]
source_files = [k.set_dir(root+'M'+str(r)+LDC_path,PS) for (r,k) in zip(R,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(R,source_files)]
target = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'figs'+PS+'viz_all_planes'+PS
test_file = 'C:'+PS+'Users'+PS+'charl'+PS+'Documents'+PS+'MOONS'+PS+'SIMS'+PS+'test'+PS+'test.dat'

symbol_markers = ['r-','bo']
# source_files = [source_files[1], source_files[2]]
# source_files = [source_files[3]]
# source_files = [source_files[7]]

for k in source_files: print(k.root.replace(root,''))
print('')

nlevels = 21

DP = [MP.my_plot() for k in source_files]

def pre_process(dp,source_file,var_name,component,line_dir,ID,PS):
	d = DF.xyz_along(line_dir)
	a = DF.xyz_adj(line_dir)
	a_i = DF.xyz_adj_ind(line_dir)
	target_dir = 'out'+PS+var_name+'_vs_'+d+PS
	if ID==10: target_file_name = 'Rm'+str(ID)
	else: target_file_name = 'Rm0'+str(ID)
	IO.directory_tree('',target_dir,PS)
	dp.set_meta(target_dir,target_file_name,d,var_name)
	dp.set_font_size(15)
	target_file = target_dir+target_file_name+'.dat'
	if (IO.does_file_exist(target_file)):
		print('file found = '+target_file)
		(d,h) = IO.get_data(target_file)
	else:
		(d,h) = f1.extract_line_from_file(source_file,line_dir)
	dp.set_data(d,h)
	# if (not IO.does_file_exist(target_file)): dp.write_data_to_file()
	dp.set_active_directions()
	dp.process_data(component)
	dp.compute_range()
	return dp

def process_dir(var_name,line_dir,ID,PS):
	d = DF.xyz_along(line_dir)
	a = DF.xyz_adj(line_dir)
	a_i = DF.xyz_adj_ind(line_dir)
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

# B
p=3
# for p in range(1,4):
for dp,sf in zip(DP,source_files):
	dp = pre_process(dp,sf.Bfield_m,DF.Bxyz_along(1),1,p,sf.ID,PS)

for dp,sf in zip(DP,source_files):
	if sf.ID>0:
		d = dp.data
		coord = d[:,0]
		Bmag = np.sqrt(pow(d[:,1],2.0)+pow(d[:,2],2.0)+pow(d[:,3],2.0))
		delta = 0.7 # 4 points used
		delta = 1 # 3 points used
		# temp = [(x,B) for x,B in zip(coord,Bmag) if x>1.05+delta and x<6.999] # Top half
		temp = [(abs(x),B) for x,B in zip(coord,Bmag) if x<-1.05-delta and x>-6.999] # Bottom half
		c_vac = [x[0] for x in temp]
		Bmag_vac = [x[1] for x in temp]
		c_vac = [np.log(x) for x in c_vac]
		Bmag_vac = [np.log(x) for x in Bmag_vac]
		x = c_vac
		y = Bmag_vac
		A = np.vstack([x, np.ones(len(x))]).T
		m, c = np.linalg.lstsq(A, y)[0]
		print('N_points_used,m = '+str(len(c_vac))+', '+str(m))
		# plt.plot(coord, Bmag, 'o', label='Original data', markersize=10)
		plt.plot(c_vac, Bmag_vac, 'o', label='Original data', markersize=10)
		y_temp = [m*t+c for t in x]
		plt.plot(x, y_temp, 'r', label='Fitted line')
		plt.legend()
		# plt.show()
		# plt.close()

IO.delete_pyc_files()
print('Done')