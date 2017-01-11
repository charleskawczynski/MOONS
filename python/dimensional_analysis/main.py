from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import file_IO as IO
import numpy as np
import os
import mat_props as mp
clear = lambda: os.system('cls')
clear()

def pow(i): return 10.0**(i)
def velocity(mat,B,L,tau): return B*L*np.sqrt(mat.sigma/(mat.rho*tau))
def Reynolds(mat,U,L): return U*L/mat.nu
def magnetic_Reynolds(mat,U,L): return U*L*mat.mu_m*mat.sigma
def Hartmann(mat,B,L): return B*L*np.sqrt(mat.sigma/(mat.rho*mat.nu))

T = 700
Li = mp.mat_props(); Li.Lithium()
LiPb = mp.mat_props(); LiPb.Lead_Lithium(T)
liq_lead = mp.mat_props(); liq_lead.liquid_lead(T)
MP = mp.mat_props();
MP = MP.Lead_lithium_Sergey()

print '***************** CHOSEN MATERIAL PROPERTY *****************'
mat = MP
# mat = Li
# mat = liq_lead
mat.print_MP()

print '\n***************** KNOWN SCALES *****************'
L = 0.1
# tau = abs(7069.62 - 45000.0)*pow(-6) # [s]
tau = abs(50.0)*pow(-6)           # [s]
# B0 = 1.00357                           # [T]
B0 = 0.010357                           # [T]
B0_T = 0.01
B0_P = 0.1

print 'L   = '+str(L)
print 'tau = '+str(tau)
print 'B0_P  = '+str(B0_P)
print 'B0_T  = '+str(B0_T)
print '\n***************** ESTIMATED SCALES *****************'
U = velocity(mat,B0_P,L,tau)
t_c = L/U
print 'U   = '+str(U)
print 't_c = '+str(t_c)
print '\n***************** DIMENSIONLESS PARAMETERS *****************'
Rem = magnetic_Reynolds(mat,U,L)
Re  = Reynolds(mat,U,L)
Ha = Hartmann(mat,B0_T,L)
print 'Rem      = '+str(Rem)
print 'Re       = '+str(Re)
print 'Rex10^-6 = '+str(Re*pow(-6))
print 'Ha       = '+str(Ha)
print '\n'



IO.delete_pyc_files()

# U = B L sqrt(sig/rho/tau)
# t = L/U
# t = sqrt(tau rho/(sig B^2))
# Rem = mu s B L^2 sqrt(sig/rho/tau)
# Ha = B L sqrt(sig/rho/nu)
# Re = (B L^2 sqrt(sig/rho/tau))/nu


# from sympy.solvers import solve
# from sympy import Symbol
# from sympy import *
# from itertools import *
# from math import factorial
# import os
# clear = lambda: os.system('cls')
# clear()
# class material_properties(sigma,nu,rho):
# 	# L8 INTRO TO MHD 1 2014 (Sergey's PPT for fusion class)
# 	print 'Units                         = [1/(Ohm m)]'
# 	print 'Weak electrolytes             = '+str(10.0**-4.0)+'-'+str(10.0**-2.0)
# 	print 'Water+25% NaCl (20 degrees C) = '+str(21.6)
# 	print 'Flibe                         = '+str(150.0)
# 	print 'Cold plasma                   = '+str(10.0**3.0)
# 	print 'Hot plasma                    = '+str(10.0**3.0)
# 	print 'LIQUID-METALS                 = '+str(10.0**6.0)+'-'+str(10.0**7.0)
# 	print 'Steel (1500 deg C)            = '+str(0.7*10.0**(7.0))
# print_electrical_conductivity()
