import file_IO as IO
import sample_uniform as SU
import os
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

path_trim = 26
PS = '\\'; print 'PS = '+PS
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'MOONS1'+PS
print 'root = '+root[path_trim:]

root = ''
name = ['KE']

# name = []
# name.append('Ufield/energy/KE')
# name.append('Bfield/energy/ME_fluid')
# name.append('Bfield/energy/ME0_fluid')
# name.append('Bfield/energy/ME1_fluid')
# name.append('Bfield/energy/ME_conductor')
# name.append('Bfield/energy/ME0_conductor')
# name.append('Bfield/energy/ME1_conductor')
# name.append('Bfield/energy/ME')
# name.append('Bfield/energy/ME0')
# name.append('Bfield/energy/ME1')
# name.append('Jfield/energy/JE')
# name.append('Jfield/energy/JE_fluid')

ext = '.dat'
file_name = [root+x+ext for x in name]

file_name_processed = [root+x+'_processed'+ext for x in name]

for (f,p) in zip(file_name,file_name_processed):
	IO.copy_file(f,p)
	SU.process(p,300)
	print 'Finished processing '+f


IO.delete_pyc_files()
print 'Done'