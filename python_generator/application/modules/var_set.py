import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'RK_Params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('n_stages','integer',priv)
	g.module[m_name].add_prop('n','integer',priv)
	g.module[m_name].add_prop('RK_active','logical',priv)
	g.module[m_name].add_prop('gamma','array',priv)
	g.module[m_name].add_prop('zeta','array',priv)
	g.module[m_name].add_prop('alpha','array',priv)
	g.module[m_name].add_prop('beta','array',priv)

	m_name = 'solver_settings'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('solve_method','integer',priv)
	g.module[m_name].add_prop('initialize','logical',priv)
	g.module[m_name].add_prop('solve','logical',priv)
	g.module[m_name].add_prop('restart','logical',priv)
	g.module[m_name].add_prop('prescribed_BCs','logical',priv)
	g.module[m_name].set_folder_name(__name__.split('.')[1])

	m_name = 'matrix_free_params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('suppress_warning','logical',priv)
	g.module[m_name].add_prop('alpha',real,priv)
	g.module[m_name].add_prop('beta',real,priv)
	g.module[m_name].add_prop('coeff_natural',real,priv)
	g.module[m_name].add_prop('coeff_explicit',real,priv)
	g.module[m_name].add_prop('coeff_implicit',real,priv)
	g.module[m_name].add_prop('coeff_implicit_time_split',real,priv)

	m_name = 'time_marching_params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('RKP','RK_Params',priv)
	g.module[m_name].add_prop('multistep_iter','integer',priv)
	g.module[m_name].add_prop('un','integer',priv)
	g.module[m_name].add_prop('n_step','integer(li)',priv)
	g.module[m_name].add_prop('n_step_stop','integer(li)',priv)
	g.module[m_name].add_prop('n_step_start','integer(li)',priv)
	g.module[m_name].add_prop('t','real(cp)',priv)
	g.module[m_name].add_prop('C_max','real(cp)',priv)
	g.module[m_name].add_prop('t_final','real(cp)',priv)
	g.module[m_name].add_prop('dt','real(cp)',priv)
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)

	m_name = 'iter_solver_params'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('un','integer',priv)
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)
	g.module[m_name].add_prop('iter_max','integer',priv)
	g.module[m_name].add_prop('tol_abs',real,priv)
	g.module[m_name].add_prop('tol_rel',real,priv)
	g.module[m_name].add_prop('iter_total','integer',priv)
	g.module[m_name].add_prop('iter_per_call','integer',priv)
	g.module[m_name].add_prop('n_skip_check_res','integer',priv)
	g.module[m_name].add_prop('export_convergence','logical',priv)
	g.module[m_name].add_prop('export_heavy','logical',priv)
	g.module[m_name].add_prop('exit_loop','logical',priv,F,1,3)

	m_name = 'export_line'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('export_ever','logical',priv)
	g.module[m_name].add_prop('dir','integer',priv)
	g.module[m_name].add_prop('line','integer',priv,F,1,2)
	g.module[m_name].add_prop('suffix','character(len=1)',priv,F,1,1)

	m_name = 'export_lines'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('EL','export_line',priv,T,1,3)
	g.module[m_name].add_prop('N','integer',priv)

	m_name = 'export_plane'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('export_ever','logical',priv)
	g.module[m_name].add_prop('dir','integer',priv)
	g.module[m_name].add_prop('plane','integer',priv,F,1,2)
	g.module[m_name].add_prop('suffix','character(len=1)',priv)

	m_name = 'export_planes'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('EP','export_plane',priv,T,1,3)
	g.module[m_name].add_prop('N','integer',priv)

	m_name = 'export_field'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('export_ever','logical',priv)

	m_name = 'var'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('IC','integer',priv)
	g.module[m_name].add_prop('BC','integer',priv)
	g.module[m_name].add_prop('SS','solver_settings',priv)
	g.module[m_name].add_prop('MFP','matrix_free_params',priv)
	g.module[m_name].add_prop('TMP','time_marching_params',priv)
	g.module[m_name].add_prop('ISP','iter_solver_params',priv)
	g.module[m_name].add_prop('unsteady_lines','export_lines',priv)
	g.module[m_name].add_prop('unsteady_planes','export_planes',priv)
	g.module[m_name].add_prop('unsteady_field','export_field',priv)

	m_name = 'var_set'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('T','var',priv)
	g.module[m_name].add_prop('U','var',priv)
	g.module[m_name].add_prop('p','var',priv)
	g.module[m_name].add_prop('B','var',priv)
	g.module[m_name].add_prop('B0','var',priv)
	g.module[m_name].add_prop('phi','var',priv)
	g.module[m_name].add_prop('rho','var',priv)

	return g
