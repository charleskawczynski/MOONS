import numpy as np

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

def compute_dot_product(a,b):
	N = a.shape
	Nx = N[0]
	Ny = N[1]
	Nz = N[2]
	c = np.zeros([Nx,Ny,Nz])
	for i in range(0,Nx):
		for j in range(0,Ny):
			for k in range(0,Nz):
				c[i,j,k] = a[i,j,k]*b[i,j,k]
	return c

def zero_outside_interior_CC_data(f_total,m_total,m_interior):
	N = f_total.shape
	Nx = N[0]
	Ny = N[1]
	Nz = N[2]
	f_interior = np.zeros([Nx,Ny,Nz])
	L = [False,False,False]
	for i in range(0,Nx):
		for j in range(0,Ny):
			for k in range(0,Nz):
				L[0] = m_total.c[0].hc_ext[i]>m_interior.c[0].hn_min and m_total.c[0].hc_ext[i]<m_interior.c[0].hn_max
				L[1] = m_total.c[1].hc_ext[j]>m_interior.c[1].hn_min and m_total.c[1].hc_ext[j]<m_interior.c[1].hn_max
				L[2] = m_total.c[2].hc_ext[k]>m_interior.c[2].hn_min and m_total.c[2].hc_ext[k]<m_interior.c[2].hn_max
				if all(L):
					f_interior[i,j,k] = f_total[i,j,k]
	return f_interior
