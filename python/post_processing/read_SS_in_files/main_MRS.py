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
import my_plot as MP
import math as m

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')

temp_dir = []
temp_dir += ['F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'MRS'+PS+'NMRS_conducting_lid_good'+PS+'NMRS1'+PS+'out'+PS+'LDC']
temp_dir += ['F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'MRS'+PS+'NMRS_conducting_lid_good'+PS+'NMRS2'+PS+'out'+PS+'LDC']
temp_dir += ['F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'MRS'+PS+'NMRS_conducting_lid_good'+PS+'NMRS3'+PS+'out'+PS+'LDC']
temp_dir += ['F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'MRS'+PS+'NMRS_conducting_lid_good'+PS+'NMRS4'+PS+'out'+PS+'LDC']
root = 'F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'MRS'+PS+'NMRS_conducting_lid_good'+PS

source_files = [MD.MOONS_directory() for k in temp_dir]
source_files = [k.set_dir(LDC_path,PS) for (LDC_path,k) in zip(temp_dir,source_files)]

for k in source_files: print(k.root.replace(root,''))
LT = []
table = []
for f in source_files:
	# KE
	f_short = f.KE
	val = IO.last_line(f_short)[1]
	LT = LT+[('KE_fluid',str(val))]
	print(f_short.replace(root,'')+' = '+str(LT[-1]))

	# # ME1_fluid
	# f_short = f.ME1_fluid
	# val = IO.last_line(f_short)[1]
	# LT = LT+[('ME1_fluid',str(val))]
	# print(f_short.replace(root,'')+' = '+str(LT[-1]))

	table.append(LT)

# row = [" VARIABLES = "]+['"'+x[0]+'"' if x==LT[-1] else '"'+x[0]+'",'+'\t\t' for x in LT]+['\n']
# print(row)
# for x in table:
# 	row = row+[c[1] if c==x[-1] else c[1]+'\t\t' for c in x]+['\n']
# row_flat = [item for sublist in row for item in sublist]
# row_flat = [' TITLE = "SS Energies"\n']+row_flat
# contents = ''.join(row_flat)
# IO.set_file_contents('TEST_NOW.dat',contents)

# IO.delete_pyc_files()
print('Done')