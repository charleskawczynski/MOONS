import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	# m_name = 'char'
	# g.add_module(m_name)
	# g.module[m_name].set_folder_name(__name__.split('.')[1])
	# g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop('character(len=1)','c',priv)

	# m_name = 'string'
	# g.add_module(m_name)
	# g.module[m_name].set_folder_name(__name__.split('.')[1])
	# g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop('char','integer',priv,T,1,3)
	# g.module[m_name].add_prop('n','integer',priv)

	return g