import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'physical_sub_domain'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('total','sub_domain',priv)
	g.module[m_name].add_prop('physical','sub_domain',priv)
	g.module[m_name].add_prop('defined','logical',priv)

	m_name = 'physical_domain'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('s','integer',priv)
	g.module[m_name].add_prop('sd','physical_sub_domain',priv,T,1,3)
	g.module[m_name].add_prop('defined','logical',priv)

	return g