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

energy = 'KE'
var = 'U'
name = []
name.append('M0'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M1'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M2'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M3'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M4'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M5'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M6'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M7'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M8'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M9'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)
name.append('M10'+PS+'out'+PS+'LDC'+PS+'unknowns'+PS+var+PS+'energy'+PS+energy)

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