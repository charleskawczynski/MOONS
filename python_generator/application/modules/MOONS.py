import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'kill_switch'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('un','integer',priv)
	g.module[m_name].add_prop('terminate_loop','logical',priv)
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)

	m_name = 'step'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('this','logical',priv)
	g.module[m_name].add_prop('next','logical',priv)

	m_name = 'export_now'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['U','B','T','rho','all'],'step',priv)
	g.module[m_name].add_prop('any_next','logical',priv)
	g.module[m_name].add_prop('any_now','logical',priv)
	g.module[m_name].add_prop('un','integer',priv)
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)

	m_name = 'refine_mesh'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop(['all','x','y','z'],'step',priv)
	g.module[m_name].add_prop('x_plane','step',priv)
	g.module[m_name].add_prop('y_plane','step',priv)
	g.module[m_name].add_prop('z_plane','step',priv)
	g.module[m_name].add_prop('any_next','logical',priv)
	g.module[m_name].add_prop('un','integer',priv)
	g.module[m_name].add_prop('i_level','integer',priv)
	g.module[m_name].add_prop('i_level_last','integer',priv)
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)
	g.module[m_name].add_prop('level','string',priv)
	g.module[m_name].add_prop('level_last','string',priv)

	m_name = 'export_safe'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('export_now','logical',priv)
	g.module[m_name].add_prop('export_period_sec',real,priv)
	g.module[m_name].add_prop('mod_period',real,priv)
	g.module[m_name].add_prop('mod_period_last',real,priv)

	m_name = 'restart_file'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('restart_input_file','logical',priv)
	g.module[m_name].add_prop('restart_fields','logical',priv)

	m_name = 'MOONS'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('mom'       ,'momentum',priv)
	g.module[m_name].add_prop('ind'       ,'induction',priv)
	g.module[m_name].add_prop('nrg'       ,'energy',priv)
	g.module[m_name].add_prop('DT'        ,'dir_tree',priv)
	g.module[m_name].add_prop('SP'        ,'sim_params',priv)
	g.module[m_name].add_prop('RF'        ,'restart_file',priv)
	g.module[m_name].add_prop('dir_target','string',priv)
	g.module[m_name].add_prop('m_temp'    ,'mesh',priv)

	return g