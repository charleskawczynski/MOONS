import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import file_IO as IO
import funcs_3D as f3
import MOONS_directory as MD

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS
print('root = '+root)
print('target = '+target)

LDC_path = PS+'out'+PS+'LDC'
source_files = [MD.MOONS_directory() for k in range(0,11)]
source_files = [source_files[k].set_dir(root+'M'+str(k)+LDC_path,PS) for k in range(0,11)]
source_files = [source_files[k].set_ID(k) for k in range(0,11)]

source_files_selected = [source_files[0],source_files[1],source_files[5],source_files[10]]

for k in source_files: print(k.root.replace(root,''))

c = f3.get_coordinates_above_tol(target+'B_Rm100-B_Rm200.dat',0.9)

IO.set_file_contents('streamtraces.mcr','\n'.join(L))


print('\n Finished')

IO.delete_pyc_files()