import sys
import os
import get_all_NEM_paths as p_NEM
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, 'IO')
sys.path.insert(0, 'funcs_1D')
sys.path.insert(0, 'convert_N_to_t')
sys.path.insert(0, 'funcs_1D')
sys.path.insert(0, 'funcs_2D')
sys.path.insert(0, 'funcs_3D')
import convert_N_to_t as CNT
import file_IO as IO
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3

################### GET SOURCE AND TARGET PATHS ############
print '################### GET SOURCE AND TARGET PATHS ############'
PS = '\\'
path_trim = 26

PS = '\\'; print 'PS = '+PS
# root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'PP_dummy'+PS; print 'root = '+root[path_trim:]
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'Numerical Experiment Matrix'+PS; print 'root = '+root[path_trim:]

print '\n'
path_rel = p_NEM.get_all_NEM_paths(PS); # print target_rel

source = [root+        k+PS for k in path_rel]
target = [root+'PP'+PS+k+PS for k in path_rel]

for s,t in zip(source,target):
	print 'PATH (s): '+s.replace(root,'')
	print 'PATH (t): '+t.replace(root,'')

################### MAKE TARGET PATHS ############
print '################### MAKE TARGET PATHS ############'
energy_path = 'PP'+PS+'energy_vs_time'+PS
path_KE = energy_path+'KE'+PS
path_ME_f = energy_path+'ME_f'+PS
path_ME_c = energy_path+'ME_c'+PS
path_ME_t = energy_path+'ME_t'+PS
path_JE = energy_path+'JE'+PS
e_budget = 'e_budget'+PS

IO.directory_tree(root,energy_path,PS)
IO.directory_tree(root,path_KE,PS)
IO.directory_tree(root,path_ME_f,PS)
IO.directory_tree(root,path_ME_c,PS)
IO.directory_tree(root,path_ME_t,PS)
IO.directory_tree(root,path_JE,PS)

################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############
print '################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############'
CNT.copy_and_scale_all(root,source,target,PS,True)
print '\n Finished 1'

################### ASSEMBLE ALL "VS t" PLOTS ############
print '################### ASSEMBLE ALL "VS t" PLOTS ############'
CNT.copy_KE_ME_to_common_folder(root,source,target,path_KE,'KE.dat','U',PS,True)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_f,'ME1_fluid.dat','B',PS,True)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_c,'ME1_conductor.dat','B',PS,True)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_t,'ME1.dat','B',PS,True)
print '\n Finished 2'

################### PRINT KE + ME SS ############
print '################### PRINT KE + ME SS ############'
CNT.print_all_SS_energy(root,root+path_KE,'U',PS)
CNT.print_all_SS_energy(root,root+path_ME_t,'B',PS)
CNT.print_all_SS_energy(root,root+path_ME_c,'B',PS)
CNT.print_all_SS_energy(root,root+path_ME_f,'B',PS)
print '\n Finished 3'

################### COMPUTE DIFFERENCE BETWEEN PV + RV ############
print '################### COMPUTE DIFFERENCE BETWEEN PV + RV ############'
# f3.compare_PV_RV(root,source,target,'B',PS)
# f3.compare_PV_RV(root,source,target,'U',PS)
# f3.compare_PV_RV(root,source,target,'J',PS)
print '\n Finished 4'

################### 1D PRIMITIVE VARIABLES PLOTS ############
print '################### 1D PRIMITIVE VARIABLES PLOTS ############'
# for i in range(1,4):
	# f1.compare_PV_RV(root,source,target,'B',i,PS)
	# f1.compare_PV_RV(root,source,target,'U',i,PS)
	# f1.compare_PV_RV(root,source,target,'J',i,PS)
print '\n Finished 5'

################### 2D PRIMITIVE VARIABLES PLOTS ############
print '################### 2D PRIMITIVE VARIABLES PLOTS ############'
# for i in range(1,4):
# 	f2.extract_plane(root,source,target,'U',i,PS)
# 	f2.extract_plane(root,source,target,'B',i,PS)
# 	f2.extract_plane(root,source,target,'J',i,PS)
print '\n Finished 6'

################### 1D ENERGY BUDGET ############
print '################### 1D ENERGY BUDGET ############'

# for i in range(1,4):
# 	f1.energy_budget_1D(root,source,target,i,PS)
print '\n Finished 7'

################### INTEGRAL ENERGY BUDGET ############
print '################### INTEGRAL ENERGY BUDGET ############'

CNT.print_all_e_budget(root,source,e_budget,PS)
print '\n Finished 8'

IO.delete_pyc_files()

