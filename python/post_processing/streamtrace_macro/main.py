import numpy as np
import sys
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, '../lib/')
import file_IO as IO
import stream_trace_rake as SR

print('################### GET FILES ############')
PS = '\\'; print('PS = '+PS+'\n')
root = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS
target = 'D:'+PS+'CHARLIE'+PS+'SIMS'+PS+'BC_full'+PS+'results_from_prospectus'+PS+'post_processed'+PS

L = SR.init_macro()
X_n_rakes = 6; Xrange = [-1,1]
Y_n_rakes = 6; Yrange = [-1,1]
Z_n_rakes = 6; Zrange = [-1,1]
X = np.linspace(Xrange[0],Xrange[1],X_n_rakes)
Y = np.linspace(Yrange[0],Yrange[1],Y_n_rakes)
Z = np.linspace(Zrange[0],Zrange[1],Z_n_rakes)
# X = [0]
# Y = [0]
Z = [-1.049]
# Z_rake = [-1.049,1.049]

for x in X:
	for y in Y:
		for z in Z:
			# L = SR.append_rake(L,x,x,y,y,Z_rake[0],Z_rake[1])
			L = SR.append_point_rake(L,x,y,z)

IO.set_file_contents('streamtraces.mcr','\n'.join(L))
# IO.set_file_contents(target+'streamtraces.mcr','\n'.join(L))


print('\n Finished')

IO.delete_pyc_files()