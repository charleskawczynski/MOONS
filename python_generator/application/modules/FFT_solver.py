import os
import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'FFT_Solver_SF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['f','res','coeff_x','coeff_y','coeff_z'],'SF',priv)
	g.module[m_name].add_prop('s','integer',priv,F,1,3)
	g.module[m_name].add_prop('dh2',real,priv,F,1,3)
	g.module[m_name].add_prop(['Nx','Ny','Nz'],'integer',priv)

	return g