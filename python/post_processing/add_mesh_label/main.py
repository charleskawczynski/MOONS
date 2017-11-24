import numpy as np
import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import file_IO as IO
import add_label as AL

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root_folder = 'F:'+PS+'Property_of_C_Kawczynski'+PS
root = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = root_folder+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS

label_font_size = 5
L = AL.init_macro()
L = AL.set_label_font_size(L,label_font_size)
X_n_rakes = 2; Xrange = [-.5,0]
Y_n_rakes = 7; Yrange = [-.8,.99]

Z_n_rakes = 3; Zrange = [-1,1]
X = np.linspace(Xrange[0],Xrange[1],X_n_rakes)
Y = np.linspace(Yrange[0],Yrange[1],Y_n_rakes)
Z = np.linspace(Zrange[0],Zrange[1],Z_n_rakes)
# X = [0]
# Y = [0]
# Z = [-1.049]
Z = [0]
# Z_rake = [-1.049,1.049]

for x in X:
	for y in Y:
		for z in Z:
			L = AL.append_label(L,x,y,z)

IO.set_file_contents('add_mesh_of_labels.mcr','\n'.join(L))
# IO.set_file_contents(target+'streamtraces.mcr','\n'.join(L))


print('\n Finished')

IO.delete_pyc_files()