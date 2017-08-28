import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

def add_modules(g,T,F,priv,real):

	m_name = 'momentum'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('suppress_warning','logical',priv)
	g.module[m_name].add_prop('m','mesh',priv)
	g.module[m_name].add_prop('PCG_P','PCG_Solver_SF',priv)
	g.module[m_name].add_prop('PCG_U','PCG_Solver_VF',priv)
	g.module[m_name].add_prop('TS','time_statistics_VF',priv)
	g.module[m_name].add_prop(['p','divU','temp_CC'],'SF',priv)
	g.module[m_name].add_prop(['U','Ustar','Unm1','U_CC','F','Fnm1','L','temp_F1','temp_F2','temp_F3','temp_E','temp_CC_VF'],'VF',priv)
	g.module[m_name].add_prop(['U_E','TF_CC','TF_CC_edge'],'TF',priv)
	g.module[m_name].add_prop(['probe_KE','probe_KE_2C','probe_divU','probe_Q'],'probe',priv)

	m_name = 'induction'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('suppress_warning','logical',priv)
	g.module[m_name].add_prop('m','mesh',priv)
	g.module[m_name].add_prop('PCG_B','PCG_Solver_VF',priv)
	g.module[m_name].add_prop('PCG_cleanB','PCG_Solver_SF',priv)
	g.module[m_name].add_prop(['U_E','temp_E_TF','temp_F1_TF','temp_F2_TF'],'TF',priv)
	g.module[m_name].add_prop(['sigmaInv_CC','divB','divJ','phi','temp_CC'],'SF',priv)
	g.module[m_name].add_prop(['F','Fnm1','L','J','temp_E','B','Bnm1','B0','B_interior','temp_F1','temp_F2','Bstar','dB0dt','temp_CC_VF','sigmaInv_edge','J_interior','curlUCrossB','CC_VF_fluid','CC_VF_sigma'],'VF',priv)
	g.module[m_name].add_prop(['probe_divB','probe_divJ','JE','JE_fluid'],'probe',priv)
	g.module[m_name].add_prop(['ME','ME_fluid','ME_conductor','probe_dB0dt','probe_B0'],'probe',priv,F,1,3)
	g.module[m_name].add_prop(['MD_fluid','MD_sigma'],'mesh_domain',priv)

	m_name = 'energy'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('suppress_warning','logical',priv)
	g.module[m_name].add_prop('m','mesh',priv)
	g.module[m_name].add_prop('PCG_T','PCG_Solver_VF',priv)
	g.module[m_name].add_prop(['T','Tnm1','temp_CC1','temp_CC2','F','Fnm1','L','divQ','Q_source'],'SF',priv)
	g.module[m_name].add_prop(['temp_F','k','U_F','U_CC','gravity','temp_CC1_VF','temp_CC2_VF'],'VF',priv)
	g.module[m_name].add_prop(['temp_CC_TF','temp_F_TF'],'TF',priv)
	g.module[m_name].add_prop('probe_divQ','probe',priv,F,1,3)
	g.module[m_name].add_prop('MD','mesh_domain',priv)

	m_name = 'density'
	g.add_module(m_name)
	g.module[m_name].set_folder_name(__name__.split('.')[1])
	g.module[m_name].set_used_modules(['IO_tools_mod'])
	g.module[m_name].add_prop('m','mesh',priv)
	g.module[m_name].add_prop('PCG_rho','PCG_Solver_VF',priv)
	g.module[m_name].add_prop(['rho','temp_CC1'],'SF',priv)
	g.module[m_name].add_prop(['temp_F','k','U_F'],'VF',priv)
	g.module[m_name].add_prop('MD','mesh_domain',priv)

	return g