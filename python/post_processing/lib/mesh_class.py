import numpy as np
import copy
import file_IO as IO
import ops_aux_funcs as ops_aux

class coordinates:
	def __init__(self):
		self.sc = 0
		self.sn = 0
		self.hn = []
		self.hc = []
		self.dhn = []
		self.dhc = []
		self.sc_ext = 0
		self.sn_ext = 0
		self.hn_ext = []
		self.hc_ext = []
		self.dhn_ext = []
		self.dhc_ext = []
		self.dAn = []
		self.dAc = []
		return

class mesh:
	def __init__(self):
		self.c = [coordinates(),coordinates(),coordinates()]
		self.vol = np.zeros(1)
		self.volume = 0
		return

def get_mesh_from_file(file_name):
	(arr,head) = IO.get_data(file_name)
	s = IO.get_shape(head)
	(x_3D,y_3D,z_3D,f) = get_3D_indexed_data(arr,s)
	m = get_mesh(x_3D,y_3D,z_3D,s)
	b = copy.deepcopy(m)
	return b

def get_mesh(x_3D,y_3D,z_3D,s):
	temp = mesh()
	m = copy.deepcopy(temp)
	for i in range(0,3): m.c.append(coordinates())
	m = init_props(m,x_3D,y_3D,z_3D,s)
	return m

def init_props(m,x_3D,y_3D,z_3D,s):
	m.c[0].hc = [0.5*(x_3D[i,0,0]+x_3D[i+1,0,0]) for i in range(0,s[0]-1)]
	m.c[1].hc = [0.5*(y_3D[0,i,0]+y_3D[0,i+1,0]) for i in range(0,s[1]-1)]
	m.c[2].hc = [0.5*(z_3D[0,0,i]+z_3D[0,0,i+1]) for i in range(0,s[2]-1)]

	m.c[0].hn = [x_3D[i,0,0] for i in range(0,s[0])]
	m.c[1].hn = [y_3D[0,i,0] for i in range(0,s[1])]
	m.c[2].hn = [z_3D[0,0,i] for i in range(0,s[2])]

	m.c[0].hn_min = np.min(m.c[0].hn)
	m.c[1].hn_min = np.min(m.c[1].hn)
	m.c[2].hn_min = np.min(m.c[2].hn)

	m.c[0].hn_max = np.max(m.c[0].hn)
	m.c[1].hn_max = np.max(m.c[1].hn)
	m.c[2].hn_max = np.max(m.c[2].hn)

	m.c[0].dhc = [m.c[0].hc[i+1] - m.c[0].hc[i] for i in range(0,s[0]-2)]
	m.c[1].dhc = [m.c[1].hc[i+1] - m.c[1].hc[i] for i in range(0,s[1]-2)]
	m.c[2].dhc = [m.c[2].hc[i+1] - m.c[2].hc[i] for i in range(0,s[2]-2)]

	m.c[0].dhn = [m.c[0].hn[i+1] - m.c[0].hn[i] for i in range(0,s[0]-1)]
	m.c[1].dhn = [m.c[1].hn[i+1] - m.c[1].hn[i] for i in range(0,s[1]-1)]
	m.c[2].dhn = [m.c[2].hn[i+1] - m.c[2].hn[i] for i in range(0,s[2]-1)]

	m.c[0].hc_ext = [m.c[0].hc[0]-m.c[0].dhc[0]]+m.c[0].hc+[m.c[0].hc[-1]+m.c[0].dhc[-1]]
	m.c[1].hc_ext = [m.c[1].hc[0]-m.c[1].dhc[0]]+m.c[1].hc+[m.c[1].hc[-1]+m.c[1].dhc[-1]]
	m.c[2].hc_ext = [m.c[2].hc[0]-m.c[2].dhc[0]]+m.c[2].hc+[m.c[2].hc[-1]+m.c[2].dhc[-1]]

	m.c[0].hn_ext = [m.c[0].hn[0]-m.c[0].dhn[0]]+m.c[0].hn+[m.c[0].hn[-1]+m.c[0].dhn[-1]]
	m.c[1].hn_ext = [m.c[1].hn[0]-m.c[1].dhn[0]]+m.c[1].hn+[m.c[1].hn[-1]+m.c[1].dhn[-1]]
	m.c[2].hn_ext = [m.c[2].hn[0]-m.c[2].dhn[0]]+m.c[2].hn+[m.c[2].hn[-1]+m.c[2].dhn[-1]]

	m.c[0].dhc_ext = [m.c[0].hc_ext[i+1] - m.c[0].hc_ext[i] for i in range(0,s[0])]
	m.c[1].dhc_ext = [m.c[1].hc_ext[i+1] - m.c[1].hc_ext[i] for i in range(0,s[1])]
	m.c[2].dhc_ext = [m.c[2].hc_ext[i+1] - m.c[2].hc_ext[i] for i in range(0,s[2])]

	m.c[0].dhn_ext = [m.c[0].hn_ext[i+1] - m.c[0].hn_ext[i] for i in range(0,s[0]+1)]
	m.c[1].dhn_ext = [m.c[1].hn_ext[i+1] - m.c[1].hn_ext[i] for i in range(0,s[1]+1)]
	m.c[2].dhn_ext = [m.c[2].hn_ext[i+1] - m.c[2].hn_ext[i] for i in range(0,s[2]+1)]

	m.c[0].dAn_interior = [x*y for x in m.c[1].dhc for y in m.c[2].dhc]
	m.c[1].dAn_interior = [x*y for x in m.c[0].dhc for y in m.c[2].dhc]
	m.c[2].dAn_interior = [x*y for x in m.c[0].dhc for y in m.c[1].dhc]

	m.c[0].dAn = np.array([x*y for x in m.c[1].dhc_ext for y in m.c[2].dhc_ext])
	m.c[1].dAn = np.array([x*y for x in m.c[0].dhc_ext for y in m.c[2].dhc_ext])
	m.c[2].dAn = np.array([x*y for x in m.c[0].dhc_ext for y in m.c[1].dhc_ext])

	m.c[0].dAn = m.c[0].dAn.reshape(s[1],s[2])
	m.c[1].dAn = m.c[1].dAn.reshape(s[0],s[2])
	m.c[2].dAn = m.c[2].dAn.reshape(s[0],s[1])

	m.c[0].sn = len(m.c[0].hn)
	m.c[1].sn = len(m.c[1].hn)
	m.c[2].sn = len(m.c[2].hn)

	m.c[0].sc = len(m.c[0].hc)
	m.c[1].sc = len(m.c[1].hc)
	m.c[2].sc = len(m.c[2].hc)

	m.c[0].sn_ext = len(m.c[0].hn_ext)
	m.c[1].sn_ext = len(m.c[1].hn_ext)
	m.c[2].sn_ext = len(m.c[2].hn_ext)

	m.c[0].sc_ext = len(m.c[0].hc_ext)
	m.c[1].sc_ext = len(m.c[1].hc_ext)
	m.c[2].sc_ext = len(m.c[2].hc_ext)

	m.c[0].dAc = [x*y for x in m.c[1].dhn for y in m.c[2].dhn]
	m.c[1].dAc = [x*y for x in m.c[0].dhn for y in m.c[2].dhn]
	m.c[2].dAc = [x*y for x in m.c[0].dhn for y in m.c[1].dhn]
	m.vol = get_cell_volume(m)
	m.vol = ops_aux.zero_ghost_points(m.vol)
	m.volume = np.sum(m.vol)
	return m

def get_3D_indexed_data(arr,s):
	x_3D = arr[:,0].reshape(s[0],s[1],s[2],order='F')
	y_3D = arr[:,1].reshape(s[0],s[1],s[2],order='F')
	z_3D = arr[:,2].reshape(s[0],s[1],s[2],order='F')
	f    = arr[:,3].reshape(s[0],s[1],s[2],order='F')
	return (x_3D,y_3D,z_3D,f)

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

def get_cell_volume(m):
	Nx = m.c[0].sc_ext
	Ny = m.c[1].sc_ext
	Nz = m.c[2].sc_ext
	vol = np.zeros([Nx,Ny,Nz])
	for i in range(0,Nx):
		for j in range(0,Ny):
			for k in range(0,Nz):
				vol[i,j,k] = m.c[0].dhn_ext[i]*m.c[1].dhn_ext[j]*m.c[2].dhn_ext[k]
	return vol

def print_mesh(m,message):
	print('----------------------------- mesh info in '+message)
	print('m.volume = '+str(m.volume))
	print('m.vol.shape = '+str(m.vol.shape))
	print('m.c[0].sc_ext = '+str(m.c[0].sc_ext))
	print('m.c[1].sc_ext = '+str(m.c[1].sc_ext))
	print('m.c[2].sc_ext = '+str(m.c[2].sc_ext))
	print('-----------------------------')

