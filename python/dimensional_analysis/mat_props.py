# Refs:
# 1) L8 INTRO TO MHD 1 2014 (Sergey's PPT for fusion class)
# 2) http://periodictable.com/Properties/A/ElectricalConductivity.an.html
# 3)
# 4)

from sympy.solvers import solve
from sympy import Symbol
from sympy import *
from itertools import *
from math import factorial
import numpy as np
import os
clear = lambda: os.system('cls')
clear()

def pow(i): return 10.0**(i)
PI = 3.141592653589793238462643383279502884197169399375105820974
mu_m0 = 4.0*PI*pow(-7) # [H/m]

class mat_props:
	sigma = 0.0
	resistivity = 0.0
	nu = 0.0
	rho = 0.0
	mu = 0.0
	Xi_m = 0.0
	K_m = 0.0
	mu_m = 0.0

	def __init__(self):
		return

	def Lithium(self):
		self.name  = 'Lithium'
		self.sigma = 1.1*pow(7) # http://periodictable.com/Properties/A/ElectricalConductivity.an.html
		self.nu    = 8.5*pow(-7)
		self.rho   = 485.0
		self.mu    = self.nu*self.rho
		self.Xi_m  = 1.4*pow(-5) # http://hyperphysics.phy-astr.gsu.edu/hbase/Tables/magprop.html
		self.K_m   = self.Xi_m+1.0
		self.mu_m  = self.K_m*mu_m0
		return self

	def Lead_Lithium(self,T): # http://oa.upm.es/11738/1/INVE_MEM_2011_103029.pdf
		# Schulz, B. "Thermophysical properties of the Li (17) Pb (83) alloy."
		# Fusion Engineering and Design 14.3 (1991): 199-205.
		if (T<508) or (T>933):
			raise Exception('Error: T not in range in Lead_Lithium.py')
		self.name        = 'PbLi'
		self.resistivity = 102.3*10**(-6)+10.54*10**(-8)*T
		self.sigma       = 1/self.resistivity
		# self.rho         = 9486.0 # Gautam
		# self.mu          = 0.0022 # Gautam
		self.rho         = 9.70*(1-72.1*10**(-6.0)*T) # For 80 at %Pb (T_m=533 K)
		self.rho         = 11.47*(1-118.0*10**(-6.0)*T) # For 100 at %Pb (T_m=600 K)
		R = 8.314 # Gas constant, check if universal or not.
		self.mu          = 0.187*np.exp(11640/(R*T))
		self.nu          = self.mu/self.rho
		self.Xi_m        = 1.4*pow(-5) # http://hyperphysics.phy-astr.gsu.edu/hbase/Tables/magprop.html
		self.K_m         = self.Xi_m+1.0
		self.mu_m        = self.K_m*mu_m0
		return self

	def liquid_lead(self,T): # http://www.iaea.org/inis/collection/NCLCollectionStore/_Public/43/095/43095088.pdf
		if (T<600) or (T>1200):
			raise Exception('Error: T not in range in liquid_lead.py')
		self.name        = 'liq_Pb'
		self.resistivity = (67.0+0.0471*T)*10.0**-8.0
		self.sigma       = 1.0/self.resistivity
		self.nu          = 4.55*10.0**(-4.0)*np.exp(1069/T)
		self.k           = 9.2 + 0.011*T
		self.rho         = 11441 - 1.2795*T
		self.mu          = self.nu*self.rho

		self.Xi_m        = 0 # not given in ref.
		self.K_m         = self.Xi_m+1.0
		self.mu_m        = self.K_m*mu_m0
		return self

	def steel(self):
		self.name = 'steel'
		self.sigma = 0.7*10**(6.0)
		# self.nu = 8.5*10.0**(-7.0)
		# self.rho = 485.0
		# self.mu = 0.0
		return self

	def mercury(self):
		self.name = 'steel'
		self.sigma = 0.7*10**(6.0)
		# self.nu = 8.5*10.0**(-7.0)
		# self.rho = 485.0
		# self.mu = 0.0
		return self

	def print_MP(self):
		t = '\t'
		print ' --------- '+self.name+' --------- '
		print '[Ohm]         = [(kg m^2)/(s^3 A^2)]'
		print '[H] = [henry] = [(kg m^2)/(s^2 A^2)]'
		print '[H/Ohm] = [s], [H] = [Ohm s]'
		print '[S] = [1/Ohm]'
		print '[m Pa = m N/m^2 = kg/(m*s^2)]'
		print ''
		print 'sigma = ' + str(self.sigma) + t+t +   ' [Ohm^-1 m^-1]'
		print 'nu    = ' + str(self.nu)    + t+t+t + ' [m^2/s]'
		print 'rho   = ' + str(self.rho)   + t+t+t + ' [kg/m^3]'
		print 'mu    = ' + str(self.mu)    + t+t +   ' [kg/(m s)]'
		print 'mu_m  = ' + str(self.mu_m)  + t +     ' [H/m]'
		print 'mu_m  = ' + str(self.mu_m)  + t +     ' [(Ohm s)/m]'
		print 'Xi_m  = ' + str(self.Xi_m)  + t+t+t + ' [1]'
		print 'K_m   = ' + str(self.K_m)   + t+t +   ' [1]'
		print ' --------------------------------- '
		return

