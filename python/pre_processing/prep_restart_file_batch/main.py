import file_IO as IO
import os
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

path_trim = 26
PS = '\\'; print 'PS = '+PS
ext = '.dat'
restart_fields = 'T' # T/F
pad = 1

# Workstation
root = 'C:'+PS+'Users'+PS+'charl'+PS+'Documents'+PS+'MOONS'+PS+'SIMS'+PS+'BC_full'+PS+'Shatrov_inspired'+PS
# Hoffman2
root = PS+'u'+PS+'project'+PS+'morley'+PS+'ckawczyn'+PS+'MOONS_out'+PS
# PC_CK
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'SIMS'+PS
print 'root = '+root[path_trim:]

# Set root group
local_dir = []
for i in range(1,10+1): local_dir.append('MOONS'+str(i)+PS)
print 'NAMES:'
print local_dir
print '\n------------------------------------------------------'

# Set restart fields
val_set = [restart_fields for i in local_dir]
f_name = [i+'restart_file' for i in local_dir]
file_set = [root+x+ext for x in f_name]
IO.modify_param(root,file_set,val_set,'restart_fields')

# Set parameters, Rem
specific_file = 'sim_params_raw'
param = 'Rem'
val_set = []
val_set.append(1.0*100)
val_set.append(2.0*100)
val_set.append(3.0*100)
val_set.append(4.0*100)
val_set.append(5.0*100)
val_set.append(6.0*100)
val_set.append(7.0*100)
val_set.append(8.0*100)
val_set.append(9.0*100)
val_set.append(10.0*100)
f_name = [i+specific_file for i in local_dir]
file_set = [root+x+ext for x in f_name]
IO.modify_param(root,file_set,val_set,param)

IO.delete_pyc_files()
print '\n\n\nDone'