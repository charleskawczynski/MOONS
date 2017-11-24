import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import convert_N_to_t as CNT
import file_IO as IO
import funcs_1D as f1
import funcs_2D as f2
import funcs_3D as f3
import MOONS_directory as MD
import write_energies_file as WEF

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
print('root = '+root)
print('target = '+target)

Re_m = [1,100,200,300,400,500,600,700,800,900,1000]
# Re_m = [0,1,100,200,300,400,500,600,700,800,900,1000]
Re_m_s = [str(x) for x in Re_m]
LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in Re_m]
source_files = [k.set_dir(root+'Rem'+str(r)+LDC_path,PS) for (r,k) in zip(Re_m,source_files)]
source_files = [k.set_ID(r) for (r,k) in zip(Re_m,source_files)]

# source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]
# source_files_selected = [source_files[1]]
source_files_selected = source_files
# source_files_selected = source_files[1:]

# print(source_files)
for k in source_files: print(k.root.replace(root,''))
# for k in source_files_selected: print(k.root)

Re_m_list = []
ME1_conductor = []
ME1_fluid = []
ME1 = []
ME1_wall = []
ME1_vacuum = []

print('################### 1D PRIMITIVE VARIABLES PLOTS ############')
for sf in source_files_selected:
	print(sf.ME1_conductor.replace(root,''))
	ME1_conductor.append(f1.get_SS_value(sf.ME1_conductor))
	ME1_fluid.append(f1.get_SS_value(sf.ME1_fluid))
	ME1.append(f1.get_SS_value(sf.ME1))

	ME1_wall.append(ME1_conductor[-1]-ME1_fluid[-1])
	ME1_vacuum.append(ME1[-1]-ME1_conductor[-1])

file_name = target+'B_energy_per_domain.dat'
var_name_list = ['Re_m','ME1','ME1_conductor','ME1_fluid','ME1_wall','ME1_vacuum']
V = [Re_m,ME1,ME1_conductor,ME1_fluid,ME1_wall,ME1_vacuum]

# var_name_list = ['Re_m','KE','ME1_conductor','ME1_fluid','ME1']
# V = [Re_m,KE,ME1_conductor,ME1_fluid,ME1]

WEF.write_energies(file_name,V,var_name_list)

print('\n Finished')

IO.delete_pyc_files()