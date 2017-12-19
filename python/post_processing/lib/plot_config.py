import matplotlib.pyplot as plt
import file_IO as IO
import pylab
class plot_config:
	def __init__(self):
		return

	def set_plot_config(self,header):
		R = range(1,4)
		self.header = header
		self.S = IO.get_shape(self.header)
		self.S_1D = tuple([k for k in self.S if k])
		self.S_2D = tuple([k for k in self.S if k])
		self.direction_1D = tuple([i for i,k in zip(R,self.S) if     k])[0]
		self.direction_2D = tuple([i for i,k in zip(R,self.S) if not k])[0]
		self.active_dimensions = [True if k else False for k in self.S]
		self.active_components = [True,True,True]
		self.n_coordinates = sum(self.active_dimensions)
		self.n_vars = int(self.header[1].count('"')/2)
		self.n_components = self.n_vars-self.n_coordinates
		self.plot_1D = self.n_coordinates==1
		self.plot_2D = self.n_coordinates==2
		self.plot_3D = self.n_coordinates==3
		if not any([self.plot_1D,self.plot_2D,self.plot_3D]): raise NameError('Not 1D,2D or 3D!')
		# self.S_reshape = reversed(self.S_2D) # Returns an iterator
		self.S_reshape = self.S_2D[::-1]
		# if self.plot_2D:
		# 	if self.direction_2D==1: self.S_reshape = (self.S_2D[1],self.S_2D[0])
		# 	elif self.direction_2D==2: self.S_reshape = (self.S_2D[1],self.S_2D[0])
		# 	elif self.direction_2D==3: self.S_reshape = (self.S_2D[1],self.S_2D[0])
		# 	else: self.S_reshape = (self.S_2D[1],self.S_2D[0])
		self.dim_char = ['x','y','z']
		self.xyz_active = [k for k,r in zip(self.dim_char,self.active_dimensions) if r]
		self.dimension_1 = 0
		self.i_component_1 = self.n_coordinates
		self.component_1 = self.n_coordinates-1
		# self.print()

	def print(self):
		print(' ----------------- plot_config ----------------- ')
		# print('header            = '+''.join(self.header))
		print('header_VAR        = '+''.join(self.header[1]))
		print('S                 = '+str(self.S))
		print('S_1D              = '+str(self.S_1D))
		print('S_2D              = '+str(self.S_2D))
		print('direction_1D      = '+str(self.direction_1D))
		print('direction_2D      = '+str(self.direction_2D))
		print('S_reshape         = '+str(self.S_reshape))
		print('active_dimensions = '+str(self.active_dimensions))
		print('active_components = '+str(self.active_components))
		print('n_coordinates     = '+str(self.n_coordinates))
		print('n_vars            = '+str(self.n_vars))
		print('n_components      = '+str(self.n_components))
		print('plot_1D           = '+str(self.plot_1D))
		print('plot_2D           = '+str(self.plot_2D))
		print('plot_3D           = '+str(self.plot_3D))
		print('dim_char          = '+str(self.dim_char))
		print('xyz_active        = '+str(self.xyz_active))
		print('dimension_1       = '+str(self.dimension_1))
		print('component_1       = '+str(self.component_1))
		print('i_component_1     = '+str(self.i_component_1))
		print(' ----------------------------------------------- ')

	def get_coordinate_indexes(self,coordinate):
		coordinate_index = coordinate-1
		if not self.active_dimensions[coordinate_index]:
			print('coordinate = '+str(coordinate))
			print('coordinate_index = '+str(coordinate_index))
			print('active_dimensions = '+str(self.active_dimensions))
			raise NameError('Coordinate does not exist in get_coordinate_indexes')
		if coordinate==1:
			return coordinate_index
		elif coordinate==2:
			if not self.active_dimensions[coordinate_index-1]:
				coordinate_index = coordinate_index-1
		elif coordinate==3:
			if not self.active_dimensions[coordinate_index-2]:
				coordinate_index = coordinate_index-1
			if not self.active_dimensions[coordinate_index-1]:
				coordinate_index = coordinate_index-1
		else: raise NameError('Bad coordinate in get_coordinate_indexes')
		return coordinate_index

	def get_component_indexes(self,component): # Assumes all components are present (most efficient way anyway)
		component_index = self.n_coordinates+component-1
		return component_index

