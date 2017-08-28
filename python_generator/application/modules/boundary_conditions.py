import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'BC_logicals'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('GFs_defined','logical',priv)
	g.module[m_name].add_prop('BCT_defined','logical',priv)
	g.module[m_name].add_prop('vals_defined','logical',priv)
	g.module[m_name].add_prop('all_Dirichlet','logical',priv)
	g.module[m_name].add_prop('all_Neumann','logical',priv)
	g.module[m_name].add_prop('all_Robin','logical',priv)
	g.module[m_name].add_prop('all_symmetric','logical',priv)
	g.module[m_name].add_prop('all_antisymmetric','logical',priv)
	g.module[m_name].add_prop('any_Dirichlet','logical',priv)
	g.module[m_name].add_prop('any_Neumann','logical',priv)
	g.module[m_name].add_prop('any_Robin','logical',priv)
	g.module[m_name].add_prop('any_symmetric','logical',priv)
	g.module[m_name].add_prop('any_antisymmetric','logical',priv)
	g.module[m_name].add_prop('any_prescribed','logical',priv)

	m_name = 'bctype'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('Dirichlet','logical',priv)
	g.module[m_name].add_prop('Neumann','logical',priv)
	g.module[m_name].add_prop('Robin','logical',priv)
	g.module[m_name].add_prop('Periodic','logical',priv)
	g.module[m_name].add_prop('symmetric','logical',priv)
	g.module[m_name].add_prop('antisymmetric','logical',priv)
	g.module[m_name].add_prop('prescribed','logical',priv)
	g.module[m_name].add_prop('defined','logical',priv)
	g.module[m_name].add_prop('meanVal',real,priv)
	g.module[m_name].add_prop('BCT','character(len=1)',priv)

	m_name = 'single_boundary'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('bct','bctype',priv)
	g.module[m_name].add_prop('b','grid_field',priv)
	g.module[m_name].add_prop('b_modified','grid_field',priv)
	g.module[m_name].add_prop('b_total','grid_field',priv)

	m_name = 'boundary'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('n','integer',priv)
	g.module[m_name].add_prop('SB','single_boundary',priv,T,1,3)
	g.module[m_name].add_prop('name','string',priv)
	g.module[m_name].add_prop('BCL','BC_logicals',priv)

	m_name = 'boundary_conditions'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('apply_BC_order','integer',priv,F,1,6)
	g.module[m_name].add_prop('BCL','BC_logicals',priv)
	g.module[m_name].add_prop('DL','data_location',priv)
	g.module[m_name].add_prop('face','boundary',priv)
	g.module[m_name].add_prop('PA_face_BCs','procedure_array',priv)
	g.module[m_name].add_prop('PA_face_implicit_BCs','procedure_array',priv)
	g.module[m_name].add_prop('f_BCs','face_SD',priv)
	# g.module[m_name].add_prop('e_BCs','edge_SD',priv) # Not yet developed
	# g.module[m_name].add_prop('c_BCs','corner_SD',priv) # Not yet developed
	# g.module[m_name].add_prop('edge','boundary',priv)
	# g.module[m_name].add_prop('PA_edges_BCs','procedure_array',priv)
	# g.module[m_name].add_prop('PA_edges_implicit_BCs','procedure_array',priv)
	# g.module[m_name].add_prop('corner','boundary',priv)
	# g.module[m_name].add_prop('PA_corners_BCs','procedure_array',priv)
	# g.module[m_name].add_prop('PA_corners_implicit_BCs','procedure_array',priv)

	return g