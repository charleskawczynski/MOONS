import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'simple_int_tensor'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('eye','integer',priv,F,1,3)

	m_name = 'mesh_props'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('int_tensor','simple_int_tensor',priv,False,1,3)
	g.module[m_name].add_prop('plane','logical',priv,True,1,3)
	g.module[m_name].add_prop('N_cells','integer',priv,True,1,3)
	g.module[m_name].add_prop('plane_any','logical',priv)
	g.module[m_name].add_prop('N_cells_tot','integer',priv)
	g.module[m_name].add_prop('volume',real,priv)
	g.module[m_name].add_prop('hmax',real,priv,F,1,3)
	g.module[m_name].add_prop('hmin',real,priv,F,1,3)
	g.module[m_name].add_prop('dhmax',real,priv,F,1,3)
	g.module[m_name].add_prop('dhmin',real,priv,F,1,3)
	g.module[m_name].add_prop('dhmax_max',real,priv)
	g.module[m_name].add_prop('dhmin_min',real,priv)

	m_name = 'mesh'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('B','block',priv,True,1,3)
	g.module[m_name].add_prop('MP','mesh_props',priv)
	g.module[m_name].add_prop('defined','logical',priv)

	m_name = 'mesh_block'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('m','mesh',priv)
	g.module[m_name].add_prop('B','block',priv)

	return g
