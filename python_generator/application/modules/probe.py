import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'probe'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('dir'      ,'string',priv)
	g.module[m_name].add_prop('name'     ,'string',priv)
	g.module[m_name].add_prop('d'        ,real,priv)
	g.module[m_name].add_prop('d_data_dt',real,priv)
	g.module[m_name].add_prop('d_amax'   ,real,priv)
	g.module[m_name].add_prop('t'        ,real,priv)
	g.module[m_name].add_prop('un'       ,'integer',priv)
	g.module[m_name].add_prop('cols'     ,'integer',priv)
	g.module[m_name].add_prop('n_step'   ,'integer(li)',priv)
	g.module[m_name].add_prop('restart'  ,'logical',priv)
	g.module[m_name].add_prop('simple'   ,'logical',priv)

	return g