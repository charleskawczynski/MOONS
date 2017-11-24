import os
import sys

class MOONS_directory:
	def __init__(self):
		return
	def set_dir(self,dir_LDC,PS):
		self.PS = PS
		self.root = dir_LDC+PS
		self.unknowns = self.root+'unknowns'+PS

		self.U = self.unknowns+'U'+PS
		self.B = self.unknowns+'B'+PS
		self.J = self.unknowns+'J'+PS

		self.Ufield_m = self.U+'field'+PS+'U_mirror_3pn.dat'
		self.Bfield_m = self.B+'field'+PS+'B_mirror_3pn.dat'
		self.Jfield_m = self.J+'field'+PS+'J_mirror_3pn.dat'

		self.Ufield_m_f = 'U_mirror_3pn.dat'
		self.Bfield_m_f = 'B_mirror_3pn.dat'
		self.Jfield_m_f = 'J_mirror_3pn.dat'

		self.Ufield_f = 'Upn.dat'
		self.Bfield_f = 'Bpn.dat'
		self.Jfield_f = 'Jpn.dat'

		self.Ufield = self.U+'field'+PS+'Upn.dat'
		self.Bfield = self.B+'field'+PS+'Bpn.dat'
		self.Jfield = self.J+'field'+PS+'Jpn.dat'

		self.KE            = self.U+'energy'+PS+'KE.dat'
		self.ME1_fluid     = self.B+'energy'+PS+'ME1_fluid.dat'
		self.ME1_conductor = self.B+'energy'+PS+'ME1_conductor.dat'
		self.ME1           = self.B+'energy'+PS+'ME1.dat'
		self.JE_fluid      = self.J+'energy'+PS+'JE_fluid.dat'
		self.JE            = self.J+'energy'+PS+'JE.dat'
		return self

	def print_dir_local(self):
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

	def print_dir(self):
		print('-------------------------------------- MOONS directory')
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

