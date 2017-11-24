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

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
print('root = '+root)

root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
macro_dir = root_folder+'charl'+PS+'Desktop'+PS+'macros'+PS+'save_image'+PS
print('root = '+root)
print('target = '+target)

Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [100,200]
# Re_m = [100]
Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000,1500,2000]
# Re_m = [100]

Re_m_s = [str(x) for x in Re_m]
Re_m_s_name = ['Rem'+x for x in Re_m_s]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+r+LDC_path,PS) for (r,k) in zip(Re_m_s_name,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

# source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]
# source_files_selected = [source_files[2]]
source_files_selected = source_files

for k in source_files: print(k.root.replace(root,''))

table = []
LT = []
for r,f in zip(Re_m_s,source_files):
	# KE
	f_short = f.KE
	val = IO.last_line(f_short)[1]
	LT = [('KE',str(val))]
	print(f_short.replace(root,'')+' = '+str(LT))

	f_short = f.ME1_fluid
	val = IO.last_line(f_short)[1]
	LT = LT+[('ME1_fluid',str(val))]
	LT = [('Rem',r)]+LT
	print(f_short.replace(root,'')+' = '+str(LT))

	table.append(LT)

row = [" VARIABLES = "]+['"'+x[0]+'"' if x==LT[-1] else '"'+x[0]+'",'+'\t\t' for x in LT]+['\n']
print(row)
for x in table:
	row = row+[c[1] if c==x[-1] else c[1]+'\t\t' for c in x]+['\n']
row_flat = [item for sublist in row for item in sublist]
row_flat = [' TITLE = "SS Energies"\n']+row_flat
contents = ''.join(row_flat)
IO.set_file_contents('test.dat',contents)

IO.delete_pyc_files()
print('Done')