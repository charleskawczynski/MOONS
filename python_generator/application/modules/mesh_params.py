import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'mesh_quality_params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('max_mesh_stretch_ratio',real,priv)
	g.module[m_name].add_prop('N_max_points_add','integer',priv)
	g.module[m_name].add_prop('N_iter','integer',priv)
	g.module[m_name].add_prop('auto_find_N','logical',priv)

	m_name = 'segment'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('N_cells','integer',priv)
	g.module[m_name].add_prop('distribution','string',priv)
	g.module[m_name].add_prop('hmax',real,priv)
	g.module[m_name].add_prop('hmin',real,priv)
	g.module[m_name].add_prop('L',real,priv)
	g.module[m_name].add_prop('tau',real,priv)
	g.module[m_name].add_prop('yc',real,priv)
	g.module[m_name].add_prop('dir','integer',priv)

	m_name = 'mesh_params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('MQP','mesh_quality_params',priv)
	g.module[m_name].add_prop('s_base','segment',priv,T,1,3)
	g.module[m_name].add_prop('s_ext','segment',priv,T,1,3)
	g.module[m_name].add_prop('N_base','integer',priv)
	g.module[m_name].add_prop('N_ext','integer',priv)

	return g