import os
clear = lambda: os.system('cls')
clear()

import funcs as f


root = []
root.append('C:\Users\Charlie\Documents\MOONS\post_processing_dummy\LDC')
root.append('C:\Users\Charlie\Documents\MOONS\post_processing_dummy\LDC')

for r in root:
	print 'processing path: '+r

	r = r+'\\'
	r = r.replace('\\','\\\\')

	f.post_process(r,r+'Ufield\\KU',r+'Ufield\\KU','.dat')
	f.post_process(r,r+'Bfield\\KBi_f',r+'Bfield\\KBi_f','.dat')
	f.post_process(r,r+'Bfield\\KBi_c',r+'Bfield\\KBi_c','.dat')
	f.post_process(r,r+'Bfield\\KBi',r+'Bfield\\KBi','.dat')

print '\n Finished'
