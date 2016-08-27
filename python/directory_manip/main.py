import sys
import os
import get_all_NEM_paths as p_NEM
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, 'IO')
import file_IO as IO

################### GET SOURCE AND TARGET PATHS ############
print '################### GET SOURCE AND TARGET PATHS ############'
PS = '\\'
path_trim = 26

PS = '\\'; print 'PS = '+PS
# root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'PP_dummy'+PS; print 'root = '+root[path_trim:]
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'Numerical Experiment Matrix'+PS; print 'root = '+root[path_trim:]

print '\n'
path_rel = p_NEM.get_all_NEM_paths(PS); # print target_rel
Rem_rel  = p_NEM.get_all_Rem_paths(PS); # print target_rel

source = [root+        k+PS for k in path_rel]
target = [root+'PP'+PS+k+PS for k in path_rel]
source_Rem = [root+        k+PS for k in Rem_rel]
target_Rem = [root+'PP'+PS+k+PS for k in Rem_rel]

for k in source: print 's='+k.replace(root,'')
for k in target: print 't='+k.replace(root,'')
print '\n'

IO.make_directory_tree_target(root,target,PS)
IO.make_directory_tree_target(root,source,PS)

IO.delete_folders_in_dir_tree(root,target,PS)
IO.delete_folders_in_dir_tree(root,source,PS)

IO.delete_files_in_dir_tree(root,target,PS)
IO.delete_files_in_dir_tree(root,source,PS)

IO.organize_Ufield(root,source,PS)
# IO.organize_Bfield(root,source,PS)
IO.organize_Jfield(root,source,PS)

IO.organize_Ufield(root,target,PS)
IO.organize_Bfield(root,target,PS)
IO.organize_Jfield(root,target,PS)



IO.delete_pyc_files()

