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

print('***************** CHOSEN MATERIAL PROPERTY *****************')
mat = MP
mat.print_MP()

print('\n***************** KNOWN SCALES *****************')
L_c = 0.1

# tau = 10.0*pow(-3) # shutdown [estimated, p. 4] http://iopscience.iop.org/article/10.1088/0029-5515/45/9/003/pdf

# tau = 100.0*pow(-6) # disruption [range,p.1064] https://journals.aps.org/rmp/pdf/10.1103/RevModPhys.66.1015
tau = 1.0*pow(-3)   # disruption [range,p.1064] https://journals.aps.org/rmp/pdf/10.1103/RevModPhys.66.1015

# tau = 200 # startup  [estimated p.547] http://iopscience.iop.org/article/10.1088/0741-3335/44/5/304/pdf
# tau = 300 # shutdown [estimated p.547] http://iopscience.iop.org/article/10.1088/0741-3335/44/5/304/pdf

# tau = 0.0001
B0_P = 1 # [T]

B_c = B0_P
print('L_c            = '+str(L_c))
print('tau            = '+str(tau))
print('B0_P           = '+str(B0_P))
print('B_c            = '+str(B_c))
print('\n***************** ESTIMATED SCALES *****************')
U_c = velocity(mat,B_c,L_c,tau)
t_c = L_c/U_c
print('U_c            = '+str(U_c))
print('t_c            = '+str(t_c))
print('\n***************** ESTIMATED DIMENSIONLESS PARAMETERS *****************')
Rem = magnetic_Reynolds(mat,U_c,L_c)
print('Rem            = '+str(Rem))

IO.delete_pyc_files()
