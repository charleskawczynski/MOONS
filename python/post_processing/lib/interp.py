import numpy as np
# import pylab
import math
import mesh as M
import math_funcs as MF
import file_IO as IO
import dir_funcs as DF
import funcs_1D as fun1D
import funcs_2D as fun2D

def get_CC_data(N):
	s = N.shape
	Nx = s[0]
	Ny = s[1]
	Nz = s[2]
	C = zeros([Nx,Ny,Nz])
	for i in range(0,Nx-1):
		for j in range(0,Ny-1):
			for k in range(0,Nz-1):
				C[i,j,k] =(N[ i , j , k ]+ \
				           N[i+1, j , k ]+ \
				           N[ i ,j+1, k ]+ \
				           N[ i , j ,k+1]+ \
				           N[ i ,j+1,k+1]+ \
				           N[i+1, j ,k+1]+ \
				           N[i+1,j+1, k ]+ \
				           N[i+1,j+1,k+1])/8.0
	return C

def face_2_CC(face,m):
	N = face.shape
	Nfx = N[0]
	Nfy = N[1]
	Nfz = N[2]
	Nx = m.c[0].sc_ext
	Ny = m.c[1].sc_ext
	Nz = m.c[2].sc_ext
	print('Nfx = '+str(Nfx))
	print('Nfy = '+str(Nfy))
	print('Nfz = '+str(Nfz))
	print('Nx = '+str(Nx))
	print('Ny = '+str(Ny))
	print('Nz = '+str(Nz))
	print('m.volume = '+str(m.volume))
	print('m.vol.shape = '+str(m.vol.shape))
	CC = np.zeros([Nx,Ny,Nz])
	c1 = Nfx==Nx+1 and Nfy==Ny and Nfz==Nz
	c2 = Nfy==Ny+1 and Nfx==Nx and Nfz==Nz
	c3 = Nfz==Nz+1 and Nfx==Nx and Nfy==Ny
	if c1:
		for i in range(0,Nx):
			for j in range(0,Ny):
				for k in range(0,Nz):
					CC[i,j,k] = 0.5*(face[i,j,k]+face[i+1,j,k])
	elif c2:
		for i in range(0,Nx):
			for j in range(0,Ny):
				for k in range(0,Nz):
					CC[i,j,k] = 0.5*(face[i,j,k]+face[i,j+1,k])
	elif c3:
		for i in range(0,Nx):
			for j in range(0,Ny):
				for k in range(0,Nz):
					CC[i,j,k] = 0.5*(face[i,j,k]+face[i,j,k+1])
	else:
		raise NameError('bad condition in face_2_CC')
	return CC

