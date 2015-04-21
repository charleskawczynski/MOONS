# This program calculates the coefficients of the 1st and 2nd
# derivative stencils made from the file "Non-uniform grid 
# stencil" in the documentation folder.

import stencilMaker as sm
import sympy as sp
from fractions import Fraction as frac
import os
clear = lambda: os.system('cls')
clear()
# frac(numerator,denominator)

dx = sp.symbols('dx')
dx_i = sp.symbols('dx_{i}')
dx_im1 = sp.symbols('dx_{i-1}')
s = sp.symbols('i')

alpha_k = -dx_im1
beta_j = dx_i

#beta_j = frac(3,2)*dx
i = s
k = s-1
j = s+1

print '***************************** FORWARD *****************************'
sm.stencilMaker(alpha_k,beta_j,i,j,k,dx,s,False)

alpha_k = -alpha_k
beta_j = -beta_j
i = s - i + 1
j = s - j + 1
k = s - k + 1

print '***************************** BACKWARD *****************************'
sm.stencilMaker(alpha_k,beta_j,i,j,k,dx,s,False)
