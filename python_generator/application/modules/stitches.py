import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'stitch'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('L','logical',priv)
	g.module[m_name].add_prop('ID','integer',priv)

	m_name = 'stitch_face'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop('st','stitch',priv,F,1,6)
	g.module[m_name].add_prop(['hmin','hmax'],'logical',priv,F,1,3)
	g.module[m_name].add_prop(['hmin_id','hmax_id'],'integer',priv,F,1,3)

	return g
