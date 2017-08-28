import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'path'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('a','string',priv)
	g.module[m_name].add_prop('r','string',priv)

	m_name = 'dir_group'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('base','path',priv)
	g.module[m_name].add_prop('field','path',priv)
	g.module[m_name].add_prop('restart','path',priv)
	g.module[m_name].add_prop('debug','path',priv)
	g.module[m_name].add_prop('energy','path',priv)
	g.module[m_name].add_prop('residual','path',priv)
	g.module[m_name].add_prop('unsteady','path',priv)
	g.module[m_name].add_prop('stats','path',priv)
	g.module[m_name].add_prop('BCs','path',priv)

	m_name = 'dir_tree'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('tar_p','path',priv)
	g.module[m_name].add_prop('out_dir','path',priv)
	g.module[m_name].add_prop('LDC','path',priv)
	g.module[m_name].add_prop('mat','path',priv)
	g.module[m_name].add_prop('meshes','path',priv)
	g.module[m_name].add_prop('BEM','path',priv)
	g.module[m_name].add_prop('wall_clock','path',priv)
	g.module[m_name].add_prop('matrix_visualization','path',priv)
	g.module[m_name].add_prop('dimensionless_params','path',priv)
	g.module[m_name].add_prop('params','path',priv)
	g.module[m_name].add_prop('ISP','path',priv)
	g.module[m_name].add_prop('TMP','path',priv)
	g.module[m_name].add_prop('EF','path',priv)
	g.module[m_name].add_prop('export_now','path',priv)
	g.module[m_name].add_prop('refine_mesh','path',priv)
	g.module[m_name].add_prop('e_budget','path',priv)
	g.module[m_name].add_prop('e_budget_N','path',priv)
	g.module[m_name].add_prop('e_budget_C','path',priv)
	g.module[m_name].add_prop('restart_sim','path',priv)
	g.module[m_name].add_prop('restart1','path',priv)
	g.module[m_name].add_prop('restart2','path',priv)
	g.module[m_name].add_prop('restart','path',priv)
	g.module[m_name].add_prop('mesh_restart','path',priv)
	g.module[m_name].add_prop('unknowns','path',priv)

	g.module[m_name].add_prop('PS','string',priv)
	g.module[m_name].add_prop('tar','string',priv)

	g.module[m_name].add_prop(['U','B','J','T','p','phi','rho','test'],'dir_group',priv)

	return g
