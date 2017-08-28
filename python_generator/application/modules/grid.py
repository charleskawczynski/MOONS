import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'array'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('f',real,priv,True,1,6)
	g.module[m_name].add_prop('N','integer',priv,F,0,0)

	m_name = 'sparse'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('L','array',priv)
	g.module[m_name].add_prop('D','array',priv)
	g.module[m_name].add_prop('U','array',priv)
	g.module[m_name].add_prop('staggered','logical',priv,F,0,0)

	m_name = 'coordinates'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('hmin',real,priv)
	g.module[m_name].add_prop('hmax',real,priv)
	g.module[m_name].add_prop('amin',real,priv)
	g.module[m_name].add_prop('amax',real,priv)
	g.module[m_name].add_prop('maxRange',real,priv)
	g.module[m_name].add_prop('dhMin',real,priv)
	g.module[m_name].add_prop('dhMax',real,priv)
	g.module[m_name].add_prop('dhc_e',real,priv)
	g.module[m_name].add_prop('dhn_e',real,priv)
	g.module[m_name].add_prop('hc_e',real,priv)
	g.module[m_name].add_prop('hn_e',real,priv)
	g.module[m_name].add_prop('sc','integer',priv)
	g.module[m_name].add_prop('sn','integer',priv)
	g.module[m_name].add_prop('N','integer',priv)
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('i_midplane','integer',priv)
	g.module[m_name].add_prop('stencils_defined','logical',priv)
	g.module[m_name].add_prop('stencils_modified','logical',priv,F,1,2)
	g.module[m_name].add_prop('stagCC2N','sparse',priv)
	g.module[m_name].add_prop('stagN2CC','sparse',priv)
	g.module[m_name].add_prop('theta','sparse',priv)
	g.module[m_name].add_prop('colCC','sparse',priv,F,1,2)
	g.module[m_name].add_prop('colN','sparse',priv,F,1,2)
	g.module[m_name].add_prop('colCC_centered','sparse',priv,F,1,2)
	g.module[m_name].add_prop('hn','array',priv)
	g.module[m_name].add_prop('hc','array',priv)
	g.module[m_name].add_prop('dhn','array',priv)
	g.module[m_name].add_prop('dhc','array',priv)

	m_name = 'grid'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('c','coordinates',priv,F,1,3)
	g.module[m_name].add_prop('volume',real,priv)
	g.module[m_name].add_prop('defined','logical',priv)

	return g