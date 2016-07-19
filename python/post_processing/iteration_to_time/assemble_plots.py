import os
clear = lambda: os.system('cls')
clear()
import numpy as np
import funcs as f
import TecUtil
import TecVals

root = 'C:\Users\Charlie\Documents\MOONS\Numerical Experiment Matrix'
root = root+'\\'

rel = []

# ###################### THICK WALL
# ###################### High Rem
rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 400 PV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 400 RV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 1000 PV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 20\Re = 1000 RV\LDC')

rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 400 PV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 400 RV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 1000 PV\LDC')
rel.append('tw = 0.5\Rem = 100\Ha = 100\Re = 1000 RV\LDC')
# ###################### Low Rem
rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 400 PV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 400 RV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 1000 PV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 20\Re = 1000 RV\LDC')

rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 400 PV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 400 RV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 1000 PV\LDC')
rel.append('tw = 0.5\Rem = 1\Ha = 100\Re = 1000 RV\LDC')

# ###################### THIN WALL
# ###################### High Rem
# rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 400 PV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 400 RV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 1000 PV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 20\Re = 1000 RV\LDC')

# rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 400 PV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 400 RV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 1000 PV\LDC')
# rel.append('tw = 0.05\Rem = 100\Ha = 100\Re = 1000 RV\LDC')
# ###################### Low Rem
# rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 400 PV\LDC') # Does not exist
# rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 400 RV\LDC') # Does not exist
# rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 1000 PV\LDC') # Does not exist
# rel.append('tw = 0.05\Rem = 1\Ha = 20\Re = 1000 RV\LDC') # Does not exist

# rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 400 PV\LDC')
# rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 400 RV\LDC')
# rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 1000 PV\LDC')
# rel.append('tw = 0.05\Rem = 1\Ha = 100\Re = 1000 RV\LDC')


target = [root+r for r in rel]

for t,r in zip(target,rel):
	print 'assembling path: '+r

	t = t+'\\'
	t = t.replace('\\','\\\\')
	i = r.replace('\\','_')
	i = i.replace(' ','')
	i = i[0:-6] + '_' + i[-6:-4]
	# print 'i = '+i

	f.post_process(t,t+'Ufield\\KU',root+'KU_'+i,i,'.dat')


print '\n Finished'
