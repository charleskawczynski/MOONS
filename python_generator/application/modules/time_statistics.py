import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'time_statistics_SF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)
	g.module[m_name].add_prop('U_sum','SF',priv)
	g.module[m_name].add_prop('U_ave','SF',priv)
	g.module[m_name].add_prop('mean_energy','probe',priv)
	g.module[m_name].add_prop('RMS','SF',priv)
	g.module[m_name].add_prop('TSP','time_statistics_params',priv)

	m_name = 'time_statistics_VF'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('dir','string',priv)
	g.module[m_name].add_prop('name','string',priv)
	g.module[m_name].add_prop('U_sum','VF',priv)
	g.module[m_name].add_prop('U_ave','VF',priv)
	g.module[m_name].add_prop('mean_energy','probe',priv)
	g.module[m_name].add_prop('RMS','VF',priv)
	g.module[m_name].add_prop('stresses','TF',priv)
	g.module[m_name].add_prop('stresses_sum','TF',priv)
	g.module[m_name].add_prop('L2_stresses','probe',priv)
	g.module[m_name].add_prop('TSP','time_statistics_params',priv)

	return g