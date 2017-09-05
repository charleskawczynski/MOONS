import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'overlap'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('i1','integer',priv,F,1,2)
	g.module[m_name].add_prop('i2','integer',priv,F,1,2)
	g.module[m_name].add_prop('iR','integer',priv)
	g.module[m_name].add_prop('success','logical',priv)

	m_name = 'sub_domain'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('C','overlap',priv,F,1,3) #  cell center
	g.module[m_name].add_prop('N','overlap',priv,F,1,3) #  node
	g.module[m_name].add_prop('M','overlap',priv,F,1,3) #  mixed
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('g_R1_id','integer',priv)
	g.module[m_name].add_prop('g_R2_id','integer',priv)

	m_name = 'index_2D'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('i','integer',priv,F,1,2)

	m_name = 'face_SD'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('s','integer',priv)
	g.module[m_name].add_prop('G'               ,'sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('G_periodic_N'    ,'sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('B'               ,'sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('I'               ,'sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('I_OPP'           ,'sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('I_OPP_periodic_N','sub_domain',priv,F,1,6)
	g.module[m_name].add_prop('i_2D','index_2D',priv,F,1,6)
	g.module[m_name].add_prop('dh',real,priv,F,1,6)
	g.module[m_name].add_prop('nhat',real,priv,F,1,6)
	g.module[m_name].add_prop('c_w',real,priv,F,1,6)
	g.module[m_name].add_prop('Robin_coeff',real,priv,F,1,6)

	return g