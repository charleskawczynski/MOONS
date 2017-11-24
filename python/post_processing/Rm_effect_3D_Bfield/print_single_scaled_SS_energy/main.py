import sys
from decimal import Decimal
import os
clear = lambda: os.system('cls')
clear()

sys.path.insert(0, 'IO')
import file_IO as IO

################### GET SOURCE AND TARGET PATHS ############
print '################### GET SOURCE AND TARGET PATHS ############'
PS = '\\'
path_trim = 26

PS = '\\'; print 'PS = '+PS

Ha = 100
Rem = 100
NME = '\NME_8'
root = 'C:\Users\Charlie\Documents\MOONS\NME_full_BC'+NME+'\out'

# \NME_7:
# root = 'C:\Users\Charlie\Documents\MOONS\NME_full_BC\mesh_refinement_study\moderate\out'

Re = 400

N = Ha**2.0/Re
mirror_scale = 2.0
KE_scale = mirror_scale
ME_scale = mirror_scale*N/Rem
JE_scale = mirror_scale*N*2.0 # (since not 1/2 j^2/sigma)
fmt = '%.5E'

print '\nSteady state energies for case:'
print root+'\n\n'

file = root + '\LDC\unknowns\U\energy\KE.dat'
(arr,header) = IO.get_data(file)
(x,y) = IO.get_vec_data_np(arr)
y_SS = KE_scale*y[-1]
print 'KE            = '+str(fmt % Decimal(y_SS))

file = root + '\LDC\unknowns\B\energy\ME1.dat'
(arr,header) = IO.get_data(file)
(x,y) = IO.get_vec_data_np(arr)
y_SS = ME_scale*y[-1]
print 'ME1           = '+str(fmt % Decimal(y_SS))
ME1 = y_SS

file = root + '\LDC\unknowns\B\energy\ME1_conductor.dat'
(arr,header) = IO.get_data(file)
(x,y) = IO.get_vec_data_np(arr)
y_SS = ME_scale*y[-1]
print 'ME1_conductor = '+str(fmt % Decimal(y_SS))
ME1_conductor = y_SS

file = root + '\LDC\unknowns\B\energy\ME1_fluid.dat'
(arr,header) = IO.get_data(file)
(x,y) = IO.get_vec_data_np(arr)
y_SS = ME_scale*y[-1]
print 'ME1_fluid     = '+str(fmt % Decimal(y_SS))
ME1_fluid = y_SS

ME1_vacuum = ME1-ME1_conductor
print 'ME1_vacuum    = '+str(fmt % Decimal(ME1_vacuum))

file = root + '\LDC\unknowns\J\energy\JE_fluid.dat'
(arr,header) = IO.get_data(file)
(x,y) = IO.get_vec_data_np(arr)
y_SS = JE_scale*y[-1]
print 'JE_fluid      = '+str(fmt % Decimal(y_SS))


IO.delete_pyc_files()

