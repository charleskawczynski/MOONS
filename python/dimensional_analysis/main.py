from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
import mat_props
clear = lambda: os.system('cls')
clear()


sigma_steel = 0.7*10**(6.0)
sigma_mercury = 0.7*10**(6.0)
sigma_Li = 2.8*10.0**(6.0)
nu_Li = 8.5*10.0**(-7.0)
rho_Li = 485.0

Li = mat_props(10,5,6,1)


# At mid-plane: B(r=850 cm,z=0 cm) [T] vs t [micro s]
dB = 1.24369 - 0.12942  # (roughly 10 times smaller, data is cut-off)
tau = 7069.62 - 45000.0
L = 

rho = 

def velocity(B,L,sig,rho,tau):
	return B*L*math.sqrt(sig/(rho*tau))

def velocity(B,L,sig,rho,tau):
	return B*L*math.sqrt(sig/(rho*tau))

def print_material():
	return

