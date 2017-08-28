import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'data_location'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('C','logical',priv)
	g.module[m_name].add_prop('N','logical',priv)
	g.module[m_name].add_prop('E','logical',priv)
	g.module[m_name].add_prop('F','logical',priv)
	g.module[m_name].add_prop('face','logical',priv)
	g.module[m_name].add_prop('edge','logical',priv)
	g.module[m_name].add_prop('CC_along','logical',priv,F,1,3)
	g.module[m_name].add_prop('N_along','logical',priv,F,1,3)
	g.module[m_name].add_prop('CC_eye','integer',priv,F,1,3)
	g.module[m_name].add_prop('N_eye','integer',priv,F,1,3)
	g.module[m_name].add_prop('defined','logical',priv)

	return g