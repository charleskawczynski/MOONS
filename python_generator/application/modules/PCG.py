import os
import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real,abstract_interfaces_path):

	# m_name = 'preconditioners'
	m_name = 'preconditioner_interfaces'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules([''])
	g.module[m_name].read_raw_lines(abstract_interfaces_path+'preconditioner_interfaces.f90')

	m_name = 'matrix_free_operators_interfaces'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules([''])
	g.module[m_name].read_raw_lines(abstract_interfaces_path+'matrix_free_operators_interfaces.f90')

	# m_name = 'norms'
	# g.add_module(m_name)
	# g.module[m_name].set_folder_name(__name__.split('.')[1])
	# g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop(['L1','L2','Linf'],real,priv)

	# m_name = 'PCG_solver_SF'
	# g.add_module(m_name)
	# g.module[m_name].set_folder_name(__name__.split('.')[1])
	# g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop(['un','un_convergence'],'integer',priv)
	# g.module[m_name].add_prop('MFP','matrix_free_params',priv)
	# g.module[m_name].add_prop(['tempk','k'],'TF',priv)
	# g.module[m_name].add_prop(['r','p','tempx','Ax','x_BC','vol','z','Minv'],'SF',priv)
	# g.module[m_name].add_prop('norm','norms',priv)
	# g.module[m_name].add_prop('ISP','iter_solver_params',priv)
	# g.module[m_name].add_prop(['dir','name'],'string',priv)
	# g.module[m_name].add_prop('prec','preconditioner_SF',priv,F,1,1,T)
	# g.module[m_name].add_prop('operator','op_SF',priv,F,1,1,T)
	# g.module[m_name].add_prop('operator_explicit','op_SF_explicit',priv,F,1,1,T)

	# m_name = 'PCG_solver_VF'
	# g.add_module(m_name)
	# g.module[m_name].set_folder_name(__name__.split('.')[1])
	# g.module[m_name].set_used_modules(['IO_tools_mod'])
	# g.module[m_name].add_prop(['un','un_convergence'],'integer',priv)
	# g.module[m_name].add_prop('MFP','matrix_free_params',priv)
	# g.module[m_name].add_prop(['tempk','k'],'TF',priv)
	# g.module[m_name].add_prop(['r','p','tempx','Ax','x_BC','vol','z','Minv'],'VF',priv)
	# g.module[m_name].add_prop('norm','norms',priv)
	# g.module[m_name].add_prop('ISP','iter_solver_params',priv)
	# g.module[m_name].add_prop(['dir','name'],'string',priv)
	# g.module[m_name].add_prop('prec','preconditioner_VF',priv,F,1,1,T)
	# g.module[m_name].add_prop('operator','op_VF',priv,F,1,1,T)
	# g.module[m_name].add_prop('operator_explicit','op_VF_explicit',priv,F,1,1,T)

	# g.set_combined_modules(['PCG_solver_SF','PCG_solver_VF'])

	return g