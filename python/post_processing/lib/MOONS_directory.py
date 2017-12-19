class MOONS_directory:
	def __init__(self):
		return

	def set_dir(self,dir_LDC,PS):
		self.PS = PS
		self.root = dir_LDC+PS
		self.unknowns = self.root+'unknowns'+PS
		self.e_budget = self.root+'e_budget'+PS

		self.E_K_budget_terms = self.root+'e_budget'+PS+'E_K_budget_terms.dat'
		self.E_M_budget_terms = self.root+'e_budget'+PS+'E_M_budget_terms.dat'

		self.e_budget_C = self.root+'e_budget'+PS+'e_budget_C'+PS
		self.B_Unsteadyc          = self.e_budget_C+'B_Unsteadyc.dat'
		self.E_K_Convectionc      = self.e_budget_C+'E_K_Convectionc.dat'
		self.E_K_Diffusionc       = self.e_budget_C+'E_K_Diffusionc.dat'
		self.E_K_Pressurec        = self.e_budget_C+'E_K_Pressurec.dat'
		self.E_M_Convectionc      = self.e_budget_C+'E_M_Convectionc.dat'
		self.E_M_Tensionc         = self.e_budget_C+'E_M_Tensionc.dat'
		self.Joule_Heatc          = self.e_budget_C+'Joule_Heatc.dat'
		self.Lorentzc             = self.e_budget_C+'Lorentzc.dat'
		self.Poyntingc            = self.e_budget_C+'Poyntingc.dat'
		self.U_Unsteadyc          = self.e_budget_C+'U_Unsteadyc.dat'
		self.Viscous_Dissipationc = self.e_budget_C+'Viscous_Dissipationc.dat'

		self.e_budget_N = self.root+'e_budget'+PS+'e_budget_N'+PS
		self.B_Unsteadyn          = self.e_budget_N+'B_Unsteadyn.dat'
		self.E_K_Convectionn      = self.e_budget_N+'E_K_Convectionn.dat'
		self.E_K_Diffusionn       = self.e_budget_N+'E_K_Diffusionn.dat'
		self.E_K_Pressuren        = self.e_budget_N+'E_K_Pressuren.dat'
		self.E_M_Convectionn      = self.e_budget_N+'E_M_Convectionn.dat'
		self.E_M_Tensionn         = self.e_budget_N+'E_M_Tensionn.dat'
		self.Joule_Heatn          = self.e_budget_N+'Joule_Heatn.dat'
		self.Lorentzn             = self.e_budget_N+'Lorentzn.dat'
		self.Poyntingn            = self.e_budget_N+'Poyntingn.dat'
		self.U_Unsteadyn          = self.e_budget_N+'U_Unsteadyn.dat'
		self.Viscous_Dissipationn = self.e_budget_N+'Viscous_Dissipationn.dat'

		self.U = self.unknowns+'U'+PS
		self.B = self.unknowns+'B'+PS
		self.J = self.unknowns+'J'+PS

		self.Ufield_m = self.U+'field'+PS+'U_mirror_3pn.dat'
		self.Bfield_m = self.B+'field'+PS+'B_mirror_3pn.dat'
		self.Jfield_m = self.J+'field'+PS+'J_mirror_3pn.dat'

		self.mesh_mom_m = self.root+'meshes'+PS+'mesh_mom_mirror.dat'
		self.mesh_sig_m = self.root+'meshes'+PS+'mesh_MD_sigma_mirror.dat'
		self.mesh_ind_m = self.root+'meshes'+PS+'mesh_ind_mirror.dat'

		self.mesh_mom = self.root+'meshes'+PS+'m_mom.dat'
		self.mesh_sig = self.root+'meshes'+PS+'m_MD_sigma.dat'
		self.mesh_ind = self.root+'meshes'+PS+'m_ind.dat'

		self.Ufield_m_f = 'U_mirror_3pn.dat'
		self.Bfield_m_f = 'B_mirror_3pn.dat'
		self.Jfield_m_f = 'J_mirror_3pn.dat'

		self.Ufield_f = 'Upn.dat'
		self.Bfield_f = 'Bpn.dat'
		self.Jfield_f = 'Jpn.dat'

		self.Ufield = self.U+'field'+PS+'Upn.dat'
		self.Bfield = self.B+'field'+PS+'Bpn.dat'
		self.Jfield = self.J+'field'+PS+'Jpn.dat'

		self.Ufield_raw_x = self.U+'restart'+PS+'Uf_x.dat'
		self.Ufield_raw_y = self.U+'restart'+PS+'Uf_y.dat'
		self.Ufield_raw_z = self.U+'restart'+PS+'Uf_z.dat'
		self.Bfield_raw_x = self.B+'restart'+PS+'Bf_x.dat'
		self.Bfield_raw_y = self.B+'restart'+PS+'Bf_y.dat'
		self.Bfield_raw_z = self.B+'restart'+PS+'Bf_z.dat'
		self.Jfield_raw_x = self.J+'restart'+PS+'Jf_x.dat'
		self.Jfield_raw_y = self.J+'restart'+PS+'Jf_y.dat'
		self.Jfield_raw_z = self.J+'restart'+PS+'Jf_z.dat'

		self.B0field_raw_x = self.B+'restart'+PS+'B0f_x.dat'
		self.B0field_raw_y = self.B+'restart'+PS+'B0f_y.dat'
		self.B0field_raw_z = self.B+'restart'+PS+'B0f_z.dat'

		self.KE            = self.U+'energy'+PS+'KE.dat'
		self.ME1_fluid     = self.B+'energy'+PS+'ME1_fluid.dat'
		self.ME1_conductor = self.B+'energy'+PS+'ME1_conductor.dat'
		self.ME1           = self.B+'energy'+PS+'ME1.dat'
		self.ME0_fluid     = self.B+'energy'+PS+'ME0_fluid.dat'
		self.ME0_conductor = self.B+'energy'+PS+'ME0_conductor.dat'
		self.ME0           = self.B+'energy'+PS+'ME0.dat'
		self.ME_fluid      = self.B+'energy'+PS+'ME_fluid.dat'
		self.ME_conductor  = self.B+'energy'+PS+'ME_conductor.dat'
		self.ME            = self.B+'energy'+PS+'ME.dat'
		self.JE_fluid      = self.J+'energy'+PS+'JE_fluid.dat'
		self.JE            = self.J+'energy'+PS+'JE.dat'
		return self

	def print_local(self):
		print('-------------------------------------- MOONS directory')
		print('Ufield_m      = '+self.Ufield_m)
		print('Bfield_m      = '+self.Bfield_m)
		print('Jfield_m      = '+self.Jfield_m)
		print('Ufield        = '+self.Ufield)
		print('Bfield        = '+self.Bfield)
		print('Jfield        = '+self.Jfield)
		print('KE            = '+self.KE)
		print('ME1_fluid     = '+self.ME1_fluid)
		print('ME1_conductor = '+self.ME1_conductor)
		print('ME1           = '+self.ME1)
		print('JE_fluid      = '+self.JE_fluid)
		print('JE            = '+self.JE)
		print('--------------------------------------')

	def print(self):
		print('-------------------------------------- MOONS directory')
		print('ID            = '+str(self.ID))
		print('Ufield_m      = '+self.Ufield_m.replace(self.root,''))
		print('Bfield_m      = '+self.Bfield_m.replace(self.root,''))
		print('Jfield_m      = '+self.Jfield_m.replace(self.root,''))
		print('Ufield        = '+self.Ufield.replace(self.root,''))
		print('Bfield        = '+self.Bfield.replace(self.root,''))
		print('Jfield        = '+self.Jfield.replace(self.root,''))
		print('KE            = '+self.KE.replace(self.root,''))
		print('ME1_fluid     = '+self.ME1_fluid.replace(self.root,''))
		print('ME1_conductor = '+self.ME1_conductor.replace(self.root,''))
		print('ME1           = '+self.ME1.replace(self.root,''))
		print('JE_fluid      = '+self.JE_fluid.replace(self.root,''))
		print('JE            = '+self.JE.replace(self.root,''))
		print('--------------------------------------')

	def set_ID(self,ID):
		self.ID = ID
		return self

