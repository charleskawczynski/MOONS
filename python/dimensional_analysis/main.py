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

def dimensional_U(U_sim,U_C): return U_sim*U_C
def dimensional_B(B_sim,B_C): return B_sim*B_C
def dimensional_J(J_sim,J_C): return J_sim*J_C

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
# B0 = 0.010357                           # [T]
B0_T = 0.01
B0_P = 0.1
B0 = 0.5*(B0_P+B0_T)
print 'L   = '+str(L)
print 'tau = '+str(tau)
print 'B0_P  = '+str(B0_P)
print 'B0_T  = '+str(B0_T)
print '\n***************** ESTIMATED SCALES *****************'
U = velocity(mat,B0_P,L,tau)
J = mat.sigma*U*B0
t_c = L/U
print 'U   = '+str(U)
print 't_c = '+str(t_c)
print '\n***************** ESTIMATED DIMENSIONLESS PARAMETERS *****************'
Rem = magnetic_Reynolds(mat,U,L)
Re  = Reynolds(mat,U,L)
Ha = Hartmann(mat,B0_T,L)
print 'Re       = '+str(Re)
print 'Rex10^-6 = '+str(Re*pow(-6))
print 'Ha       = '+str(Ha)
print 'Rem      = '+str(Rem)

print '\n***************** DIMENSIONALIZING PARAMETERS *****************'

U_sim = np.sqrt(pow(-16))
B_sim = np.sqrt(0.01)
J_sim = np.sqrt(0.57)
U_dim = dimensional_U(U_sim,U)
B_dim = dimensional_B(B_sim,B0)
J_dim = dimensional_B(J_sim,J)
print 'U_sim [   ]   = '+str(U_sim)
print 'B_sim [   ]   = '+str(B_sim)
print 'J_sim [   ]   = '+str(J_sim)
print 'U_dim [m/s]   = '+str(U_dim)
print 'B_dim [ T ]   = '+str(B_dim)
print 'J_dim [ A ]   = '+str(J_dim)

print '\n***************** ACTUAL DIMENSIONLESS PARAMETERS *****************'
Rem = magnetic_Reynolds(mat,U_dim,L)
Re  = Reynolds(mat,U_dim,L)
Ha = Hartmann(mat,B0_T,L)
print 'Re        (real)  = '+str(Re)
print 'Rex10^-6  (real)  = '+str(Re*pow(-6))
print 'Ha        (real)  = '+str(Ha)
print 'Rem       (real)  = '+str(Rem)


IO.delete_pyc_files()
