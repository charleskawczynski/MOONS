import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'bctype'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['Dirichlet','Neumann','Robin','Periodic','symmetric','antisymmetric','prescribed','defined'],'logical',priv)
	g.module[m_name].add_prop('meanVal',real,priv)
	g.module[m_name].add_prop('BCT','character(len=1)',priv)

	m_name = 'data_location'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['C','N','E','F','defined'],'logical',priv)
	g.module[m_name].add_prop(['face','edge','volume_ID'],'integer',priv)
	g.module[m_name].add_prop(['CC_along','N_along'],'logical',priv,F,1,3)
	g.module[m_name].add_prop(['CC_eye','N_eye'],'integer',priv,F,1,3)

	return g
