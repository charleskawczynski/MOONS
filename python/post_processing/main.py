import os
import funcs as f
import get_all_NEM_paths as p_NEM
clear = lambda: os.system('cls')
clear()


################### GET SOURCE AND TARGET PATHS ############
print '################### GET SOURCE AND TARGET PATHS ############'
PS = '\\'
path_trim = 26

PS = '\\'; print 'PS = '+PS
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'PP_dummy'+PS; print 'root = '+root[path_trim:]
# root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'Numerical Experiment Matrix'+PS; print 'root = '+root[path_trim:]
print '\n'
source_rel = p_NEM.get_all_NEM_paths(PS); # print source_rel
target_rel = p_NEM.get_all_NEM_paths(PS); # print target_rel

source = [root+        k+PS for k in source_rel]
target = [root+'PP'+PS+k+PS for k in target_rel]
for k in source: print 's='+k[path_trim:]
for k in target: print 't='+k[path_trim:]
print '\n'

for s,t in zip(source,target):
	print 'PATH (s): '+s[path_trim:]
	print 'PATH (t): '+t[path_trim:]

################### MAKE TARGET PATHS ############
print '################### MAKE TARGET PATHS ############'

f.make_directory_tree_target(root,source,target,PS)

################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############
print '################### CONVERT OLD "VS N" TO "VS t" TO NEW DIRECTORY ############'

# f.convert_N_to_t_all(root,source,target,PS)
print '\n Finished 1'

################### ASSEMBLE ALL "VS t" PLOTS ############
print '################### ASSEMBLE ALL "VS t" PLOTS ############'

f.copy_KE_ME_to_common_folder(root,source,target,'KU_vs_t.dat','U',PS)
print '\n Finished 2'

################### COMPUTE DIFFERENCE BETWEEN PV + RV ############
print '################### COMPUTE DIFFERENCE BETWEEN PV + RV ############'

# f.compare_PV_RV(root,source,target,'B',PS)
# f.compare_PV_RV(root,source,target,'U',PS)

print '\n Finished 3'

################### 1D PLOTS ############
print '################### 1D PLOTS ############'

# f.compare_PV_RV_1D(root,source,target,'B',PS)
print '\n Finished 4'

################### PLOT KE + ME ############
print '################### PLOT KE + ME ############'
f.plot_all_files_in_path(root+'PP'+PS,'TKE','Time',PS)
print '\n Finished 5'


