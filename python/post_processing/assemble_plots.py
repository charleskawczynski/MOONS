import os
clear = lambda: os.system('cls')
clear()
import numpy as np
import funcs as f
import TecUtil
import TecVals

# root = 'C:\Users\Charlie\Documents\MOONS\Numerical Experiment Matrix'; root = root+'\\'
# rel = get_all_NEM_paths() # Returns all Numerical Experiment Matrix relative paths to LDC

root = 'C:\Users\Charlie\Documents\MOONS'; root = root+'\\'
rel = []
rel.append('post_processing_dummy\LDC')


target = [root+r for r in rel]

for t,r in zip(target,rel):
	print 'assembling path: '+r

	t = t+'\\'
	t = t.replace('\\','\\\\')
	i = r.replace('\\','_')
	i = i.replace(' ','')
	i = i[0:-6] + '_' + i[-6:-4]
	# print 'i = '+i

	f.convert_N_to_t(t,t+'Ufield\\KU',root+'KU_'+i,i,'.dat')


print '\n Finished'
