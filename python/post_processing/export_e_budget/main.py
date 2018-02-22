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
import write_energies_to_file as WEF

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = 'F:'+PS+'Property_of_C_Kawczynski'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
print('root = '+root)

# LDC_path = PS+'out'+PS+'LDC'
# R = range(0,10+1)
# source_files = [MD.MOONS_directory() for k in R]
# source_files = [k.set_dir(root+'M'+str(r)+LDC_path,PS) for (r,k) in zip(R,source_files)]
# # source_files = [k.set_dir(root+'NMRS'+str(r)+LDC_path,PS) for (r,k) in zip(R,source_files)]
# source_files = [k.set_ID(r) for (r,k) in zip(R,source_files)]

Re_m = [1,100,200,300,400,500,600,700,800,900,1000,1500,2000]
# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
Re_m_s = [str(x) for x in Re_m]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+'Rem'+str(r)+LDC_path,PS) for (r,k) in zip(Re_m,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]


for k in source_files: print(k.root.replace(root,''))

table = []
for r,f in zip(Re_m,source_files):
	f_short = f.E_K_budget_terms
	# f_short = f.E_M_budget_terms
	LT = IO.get_e_budget(f_short)
	LT = [('Rem',str(r))]+LT
	print(f_short.replace(root,''))
	table.append(LT)

row = [" VARIABLES = "]+['"'+x[0]+'"' if x==LT[-1] else '"'+x[0]+'",'+'\t\t' for x in LT]+['\n']
for x in table:
	row = row+[c[1] if c==x[-1] else c[1]+'\t\t' for c in x]+['\n']
row_flat = [item for sublist in row for item in sublist]
row_flat = [' TITLE = "SS Energies"\n']+row_flat
contents = ''.join(row_flat)

file_name = target+'E_K_budget_terms.dat'
IO.set_file_contents(file_name,contents)


file_name = target+'B_energy_per_domain.dat'
var_name_list = ['Re_m','ME1','ME1_conductor','ME1_fluid','ME1_wall','ME1_vacuum']
V = [Re_m,ME1,ME1_conductor,ME1_fluid,ME1_wall,ME1_vacuum]

WEF.write_energies_to_file(file_name,V,var_name_list)


IO.delete_pyc_files()
print('Done')