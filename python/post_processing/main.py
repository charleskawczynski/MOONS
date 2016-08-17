import sys
import os
import get_all_NEM_paths as p_NEM
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, 'IO')
sys.path.insert(0, 'funcs_1D')
sys.path.insert(0, 'convert_N_to_t')
sys.path.insert(0, 'funcs_1D')
# sys.path.insert(0, 'funcs_2D')
sys.path.insert(0, 'funcs_3D')
import convert_N_to_t as CNT
import file_IO as IO
import funcs_1D as f1
# import funcs_2D as f2
import funcs_3D as f3

################### GET SOURCE AND TARGET PATHS ############
print '################### GET SOURCE AND TARGET PATHS ############'
PS = '\\'
path_trim = 26

PS = '\\'; print 'PS = '+PS
# root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'PP_dummy'+PS; print 'root = '+root[path_trim:]
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'Numerical Experiment Matrix'+PS; print 'root = '+root[path_trim:]

print '\n'
source_rel = p_NEM.get_all_NEM_paths(PS); # print source_rel
target_rel = p_NEM.get_all_NEM_paths(PS); # print target_rel

source = [root+        k+PS for k in source_rel]
target = [root+'PP'+PS+k+PS for k in target_rel]
for k in source: print 's='+k.replace(root,'')
for k in target: print 't='+k.replace(root,'')
print '\n'

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
IO.make_directory_tree_target(root,source,target,PS)
IO.directory_tree(root,energy_path,PS)
IO.directory_tree(root,path_KE,PS)
IO.directory_tree(root,path_ME_f,PS)
IO.directory_tree(root,path_ME_c,PS)
IO.directory_tree(root,path_ME_t,PS)

################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############
print '################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############'
# CNT.convert_N_to_t_all(root,source,target,PS)
print '\n Finished 1'

################### ASSEMBLE ALL "VS t" PLOTS ############
print '################### ASSEMBLE ALL "VS t" PLOTS ############'
CNT.copy_KE_ME_to_common_folder(root,source,target,path_KE,'KE_vs_t.dat','U',PS)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_f,'MEi_f_vs_t.dat','B',PS)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_c,'MEi_c_vs_t.dat','B',PS)
CNT.copy_KE_ME_to_common_folder(root,source,target,path_ME_t,'MEi_t_vs_t.dat','B',PS)
print '\n Finished 2'

################### PLOT KE + ME VS TIME ############
print '################### PLOT KE + ME VS TIME ############'
# CNT.plot_all_files_in_path(root,root+path_KE,'KE','Time','U',PS)
# CNT.plot_all_files_in_path(root,root+path_ME_t,'ME','Time','B',PS)
print '\n Finished 3'

################### PRINT KE + ME SS ############
print '################### PRINT KE + ME SS ############'
CNT.print_all_SS_energy(root,root+path_KE,'U',PS)
CNT.print_all_SS_energy(root,root+path_ME_f,'B',PS)
CNT.print_all_SS_energy(root,root+path_ME_c,'B',PS)
CNT.print_all_SS_energy(root,root+path_ME_t,'B',PS)
print '\n Finished 4'

################### COMPUTE DIFFERENCE BETWEEN PV + RV ############
print '################### COMPUTE DIFFERENCE BETWEEN PV + RV ############'
# f3.compare_PV_RV(root,source,target,'B',PS)
# f3.compare_PV_RV(root,source,target,'U',PS)
# f3.compare_PV_RV(root,source,target,'J',PS)
print '\n Finished 5'

################### 1D PRIMITIVE VARIABLES PLOTS ############
print '################### 1D PRIMITIVE VARIABLES PLOTS ############'
# for i in range(1,4):
	# f1.compare_PV_RV(root,source,target,'B',i,PS)
	# f1.compare_PV_RV(root,source,target,'U',i,PS)
	# f1.compare_PV_RV(root,source,target,'J',i,PS)
print '\n Finished 6'

################### 2D PRIMITIVE VARIABLES PLOTS ############
print '################### 2D PRIMITIVE VARIABLES PLOTS ############'
# for i in range(1,4):
	# f2.compare_PV_RV(root,source,target,'B',i,PS)
	# f2.compare_PV_RV(root,source,target,'U',i,PS)
	# f2.compare_PV_RV(root,source,target,'J',i,PS)
print '\n Finished 7'

IO.delete_pyc_files()

