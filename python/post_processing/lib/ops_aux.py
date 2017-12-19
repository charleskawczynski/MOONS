import numpy as np
import mesh as M

def zero_ghost_points(f):
	N = f.shape
	Nx = N[0]
	Ny = N[1]
	Nz = N[2]
	L = [False,False,False,False,False,False]
	for i in range(0,Nx):
		for j in range(0,Ny):
			for k in range(0,Nz):
				L[0] = i==0
				L[1] = j==0
				L[2] = k==0
				L[3] = i==Nx-1
				L[4] = j==Ny-1
				L[5] = k==Nz-1
				if any(L):
					f[i,j,k] = 0.0
	return f
