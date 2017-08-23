import math
from math import log10,floor

def round_nearest(x, a):
	return round(x / a) * a

def round_to(n, precision):
	correction = 0.5 if n >= 0 else -0.5
	return int( n/precision+correction ) * precision

def round_to_n(x, n):
	round_to_n = lambda x, n: round(x, -int(floor(log10(x))) + (n - 1))
	return round_to_n

def round_sig(x, sig=2):
	return round(x, sig-int(floor(log10(abs(x))))-1)
