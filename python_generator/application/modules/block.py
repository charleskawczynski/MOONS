import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'block'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('g','grid',priv,F,1,3)
	g.module[m_name].add_prop('f','grid',priv,T,1,3)
	g.module[m_name].add_prop('e','grid',priv,T,1,3)
	g.module[m_name].add_prop('c','grid',priv,T,1,3)
	g.module[m_name].add_prop('fb','grid',priv,T,1,3)
	g.module[m_name].add_prop('eb','grid',priv,T,1,3)
	g.module[m_name].add_prop('cb','grid',priv,T,1,3)
	g.module[m_name].add_prop('vol','grid_field',priv,T,1,6)
	g.module[m_name].add_prop('apply_BC_order','integer',priv,F,1,6)

	return g
