from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
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

Li = mp.mat_props(); Li.Lithium()

print '***************** CHOSEN MATERIAL PROPERTY *****************'
mat = Li
mat.print_MP()

print '\n***************** KNOWN SCALES *****************'
L_r = 40.0*pow(-2)                   # [m]
L_p = 200.0*pow(-2)                  # [m]
# L_r = 5.0*pow(-2)                   # [m]
L = L_r/2
# tau = abs(7069.62 - 45000.0)*pow(-6) # [s]
tau = abs(45000.0)*pow(-6)           # [s]
B0 = 1.00357                           # [T]
print 'L   = '+str(L)
print 'tau = '+str(tau)
print 'B0  = '+str(B0)
print '\n***************** ESTIMATED SCALES *****************'
B1 = B0*0.1                          # [T]
# B1 = B0*0.1                          # [T]
B = B0+B1
U = velocity(mat,B0,L,tau)
t_c = L/U
print 'U   = '+str(U)
print 't_c = '+str(t_c)
print '\n***************** DIMENSIONLESS PARAMETERS *****************'
Rem = magnetic_Reynolds(mat,U,L)
Re  = Reynolds(mat,U,L)
Ha  = Hartmann(mat,B,L)
Ha0 = Hartmann(mat,B0,L)
print 'Rem      = '+str(Rem)
print 'Re       = '+str(Re)
print 'Rex10^-6 = '+str(Re*pow(-6))
print 'Ha       = '+str(Ha)
print 'Ha0      = '+str(Ha0)
print '\n'
print 'B0*Rem = '+str(B0*Rem)



