from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import os
clear = lambda: os.system('cls')
clear()

class material_properties:
	sigma
	nu
	rho
	mu

	def __init__(self,sigma,nu,rho,mu):
		self.sigma = sigma
		self.nu = nu
		self.rho = rho
		self.mu = mu
