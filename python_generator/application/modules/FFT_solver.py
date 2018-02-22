import os
import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'FFT_Solver_SF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod','norms_mod'])
	g.module[m_name].add_prop(['f','res','vol','coeff'],'SF',priv)
	g.module[m_name].add_prop('direction','integer',priv)
	g.module[m_name].add_prop('norm','norms',priv)
	g.module[m_name].add_prop('var_name','string',priv)

	return g