import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'SF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('BF','block_field',priv,T,1,3)
	g.module[m_name].add_prop('DL','data_location',priv)
	g.module[m_name].add_prop('all_neumann','logical',priv)
	g.module[m_name].add_prop('s','integer',priv)
	g.module[m_name].add_prop('numEl','integer',priv)
	g.module[m_name].add_prop('numPhysEl','integer',priv)
	g.module[m_name].add_prop('vol',real,priv)

	m_name = 'VF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('x','SF',priv)
	g.module[m_name].add_prop('y','SF',priv)
	g.module[m_name].add_prop('z','SF',priv)

	m_name = 'TF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('x','VF',priv)
	g.module[m_name].add_prop('y','VF',priv)
	g.module[m_name].add_prop('z','VF',priv)

	return g