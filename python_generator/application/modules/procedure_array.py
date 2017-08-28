import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'single_procedure'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('P','apply_face_BC_op',priv,F,1,1,T)
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('ID','integer',priv)

	m_name = 'single_procedure_plane_op'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('P','plane_op',priv,F,1,1,T)
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('ID','integer',priv)

	m_name = 'procedure_array'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('N','integer',priv)
	g.module[m_name].add_prop('SP','single_procedure',priv,T,1,3)
	g.module[m_name].add_prop('defined','logical',priv)

	m_name = 'procedure_array_plane_op'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('N','integer',priv)
	g.module[m_name].add_prop('SP','single_procedure_plane_op',priv,T,1,3)
	g.module[m_name].add_prop('defined','logical',priv)

	return g