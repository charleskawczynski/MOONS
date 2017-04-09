import file_IO as IO
import os
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

path_trim = 26
PS = '\\'; print 'PS = '+PS
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'MOONS1'+PS
root = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'MOONS'+PS+'MOONS1'+PS
root = 'C:'+PS+'Users'+PS+'charl'+PS+'Documents'+PS+'MOONS'+PS+'SIMS'+PS+'BC_full'+PS+'Shatrov_inspired'+PS
print 'root = '+root[path_trim:]

e_file = 'KE'
var = 'U'
name = []
path_ending = PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+e_file
name.append('M0'+path_ending)
name.append('M1'+path_ending)
name.append('M2'+path_ending)
name.append('M3'+path_ending)
name.append('M4'+path_ending)
name.append('M5'+path_ending)
name.append('M6'+path_ending)
name.append('M7'+path_ending)
name.append('M8'+path_ending)
name.append('M9'+path_ending)
name.append('M10'+path_ending)

ext = '.dat'
file_name = [root+x+ext for x in name]

for f in file_name:
	fc = IO.file_get_contents(f)
	LL = IO.last_line(f)
	f_short = f.replace(root,'')
	# print 'Finished processing '+f_short+' = '+str(LL[1])
	print 'energy = '+str(LL[1])


IO.delete_pyc_files()
print 'Done'