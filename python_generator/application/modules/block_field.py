import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'block_field'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('GF','grid_field',priv)
	g.module[m_name].add_prop('BCs','boundary_conditions',priv)
	g.module[m_name].add_prop('DL','data_location',priv)
	g.module[m_name].add_prop('many_cell_N_periodic','logical',priv,F,1,3)
	g.module[m_name].add_prop('many_cell','logical',priv,F,1,3)
	# g.module[m_name].add_prop('st','stitches',priv)
	g.module[m_name].add_prop('PA_assign_ghost_XPeriodic','procedure_array_plane_op',priv)
	g.module[m_name].add_prop('PA_assign_ghost_N_XPeriodic','procedure_array_plane_op',priv)
	g.module[m_name].add_prop('PA_assign_wall_Dirichlet','procedure_array_plane_op',priv)
	g.module[m_name].add_prop('PA_assign_wall_Periodic_single','procedure_array_plane_op',priv)
	g.module[m_name].add_prop('PA_multiply_wall_Neumann','procedure_array_plane_op',priv)

	return g