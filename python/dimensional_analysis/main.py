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
def KE_dimensional(mat,U,L,KE_dimensionless): return (U**2*mat.rho *L**3)*KE_dimensionless
def ME_dimensional(mat,B,L,ME_dimensionless): return (B**2/mat.mu_m*L**3)*ME_dimensionless

def dimensional_U(U_sim,U_C): return U_sim*U_C
def dimensional_B(B_sim,B_C): return B_sim*B_C
def dimensional_J(J_sim,J_C): return J_sim*J_C

T = 700
Li = mp.mat_props(); Li.Lithium()
LiPb = mp.mat_props(); LiPb.Lead_Lithium(T)
liq_lead = mp.mat_props(); liq_lead.liquid_lead(T)
MP = mp.mat_props(); MP = MP.Lead_lithium_Sergey()

print '***************** CHOSEN MATERIAL PROPERTY *****************'
mat = MP
# mat = Li
# mat = liq_lead
mat.print_MP()

print '\n***************** KNOWN SCALES *****************'
L_c = 0.1
# tau = abs(7069.62 - 45000.0)*pow(-6)    # [s] - Based on Mike Ulrickson's data
tau = 0.0047                            # [s] - Based on B0/max(dB0/dt)
# tau = 0.0047
# tau = abs(50.0)*pow(-6)                 # [s] - Sergey's suggestion
B0_T = 5 # [T]
B0_P = 1 # [T]
B_c = B0_P
print 'L_c            = '+str(L_c)
print 'tau            = '+str(tau)
print 'B0_P           = '+str(B0_P)
print 'B0_T           = '+str(B0_T)
print 'B_c            = '+str(B_c)
print '\n***************** ESTIMATED SCALES *****************'
U_c = velocity(mat,B_c,L_c,tau)

J_c = mat.sigma*U_c*B_c
t_c = L_c/U_c
print 'U_c            = '+str(U_c)
print 'J_c            = '+str(J_c)
print 't_c            = '+str(t_c)
print '\n***************** ESTIMATED DIMENSIONLESS PARAMETERS *****************'
Rem = magnetic_Reynolds(mat,U_c,L_c)
Re  = Reynolds(mat,U_c,L_c)
Ha  = Hartmann(mat,B_c,L_c)
print 'Re             = '+str(Re)
print 'Ha             = '+str(Ha)
print 'Rem            = '+str(Rem)

print '\n************ DIMENSIONLESS PARAMETERS FROM MOONS OUTPUT ************'
U_sim = np.sqrt(0.5)
B_sim = np.sqrt(0.1)
J_sim = np.sqrt(0.57) # J^2 does not include 1/2
KE_sim = 0.25
ME_sim = 0.25
print ' U_sim [   ]   = '+str(U_sim)
print ' B_sim [   ]   = '+str(B_sim)
print ' J_sim [   ]   = '+str(J_sim)
print 'KE_sim [   ]   = '+str(KE_sim)
print 'ME_sim [   ]   = '+str(ME_sim)
print '\n************ DIMENSIONALIZING PARAMETERS FROM MOONS OUTPUT ************'
U_dim = dimensional_U(U_sim,U_c)
B_dim = dimensional_B(B_sim,B_c)
J_dim = dimensional_B(J_sim,J_c)
KE_dim = KE_dimensional(mat,U_c,L_c,KE_sim)
ME_dim = ME_dimensional(mat,B_c,L_c,ME_sim)
print ' U_dim [m/s]   = '+str(U_dim)
print ' B_dim [ T ]   = '+str(B_dim)
print ' J_dim [ A ]   = '+str(J_dim)
print 'KE_dim [ J ]   = '+str(KE_dim)
print 'ME_dim [ J ]   = '+str(ME_dim)
print '\n****************** ACTUAL DIMENSIONLESS PARAMETERS *******************'
Rem = magnetic_Reynolds(mat,U_dim,L_c)
Re  = Reynolds(mat,U_dim,L_c)
Ha = Hartmann(mat,B_c,L_c)
print 'Re     (real)  = '+str(Re)
print 'Ha     (real)  = '+str(Ha)
print 'Rem    (real)  = '+str(Rem)

IO.delete_pyc_files()
