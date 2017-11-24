import math
from math import log10,floor

def round_nearest(x, a):
	return round(x / a) * a

def round_to(n, precision):
	# precision, e.g., may be 1e-2
	correction = 0.5 if n >= 0 else -0.5
	return int( n/precision+correction ) * precision

def round_to_n(x, n):
	round_to_n = lambda x, n: round(x, -int(floor(log10(x))) + (n - 1))
	return round_to_n

def round_sig(x, sig=2):
	if abs(x)<1e-15:
		R = 0.0
	else:
		try:
			temp = abs(x)
			temp_L = log10(temp)
			temp_F = floor(temp_L)
			temp_I = int(temp_F)
			R = round(x, sig-temp_I-1)
		except:
			print('------------------------------------- round warning')
			print('x = '+str(x))
			R = 0.0
			print('R = '+str(R))
			print('-------------------------------------')
	return R


def convert_inf_nan_to_zero(f):
	for i in range(0,len(f)):
		if f[i]== float('NaN'): f[i] = 0.0
		if f[i]==-float('NaN'): f[i] = 0.0
		if f[i]== float('inf'): f[i] = 0.0
		if f[i]==-float('inf'): f[i] = 0.0
		if math.isnan(f[i]): f[i] = 0.0
	return f

def round_range(R):
	round_range_L = True
	tol = pow(10.0,-8.0)
	if round_range_L:
		for k in R:
			try: k = MF.round_sig(k,2)
			except: k = 0
		if abs(R[0]-R[1])<tol:
			R = [-1,1]
	return R
