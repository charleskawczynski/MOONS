import matplotlib.pyplot as plt
import file_IO as IO
import plot_config as PC
import numpy as np
import pylab
import math_funcs as MF
class my_plot:
	def __init__(self):
		return

	def set_meta(self,target_dir,target_file_name,xlabel,ylabel,zlabel = None):
		self.font_size = 12
		self.font = {'family' : 'monospace', 'weight' : 'bold', 'size' : self.font_size}
		self.define_lim = False
		self.range_set = False
		self.define_legend = False
		self.target_dir = target_dir
		self.target_file_name = target_file_name
		self.target_file = target_dir+target_file_name
		self.xlabel = xlabel
		self.ylabel = ylabel
		self.zlabel = zlabel
		return self

	def set_data(self,data,header):
		self.data = data
		self.header = header
		self.S = IO.get_shape(self.header)

	def write_data_to_file(self):
		IO.save_data_to_file(self.target_file+'.dat',self.data,self.header)

	def set_active_directions(self):
		self.PC = PC.plot_config()
		self.PC.set_plot_config(self.header)

	def process_data(self,component_in = 1):
		if self.PC.plot_1D:
			coordinate_index = self.PC.get_coordinate_indexes(self.PC.direction_1D)
			component_index = self.PC.get_component_indexes(component_in)
			if coordinate_index==component_index: Error('coordinate may not = component index')
			self.x = self.data[:,coordinate_index]
			self.y = self.data[:,component_index]
		elif self.PC.plot_2D:
			coordinate_x = self.PC.direction_2D[0]
			coordinate_y = self.PC.direction_2D[1]
			self.x = self.data[:,coordinate_x]
			self.y = self.data[:,coordinate_y]
			self.z = self.data[:,component]
			self.X = self.x.reshape(self.PC.S_reshape)
			self.Y = self.y.reshape(self.PC.S_reshape)
			self.Z = self.z.reshape(self.PC.S_reshape)

	def compute_range(self):
		self.range_set = True
		self.xRange = [np.min(self.x), np.max(self.x)]
		self.yRange = [np.min(self.y), np.max(self.y)]
		if self.PC.plot_2D:
			self.zRange = [np.min(self.z), np.max(self.z)]
		else:
			self.zRange = self.yRange

	def set_range(self,xyz_range,nlevels):
		self.range_set = True
		self.nlevels = nlevels
		self.xRange = xyz_range[0]
		self.yRange = xyz_range[1]
		if self.PC.plot_2D:
			self.zRange = xyz_range[2]
			self.level_step = (self.zRange[1]-self.zRange[0])/self.nlevels
			self.level_step = MF.round_sig(self.level_step, 2)
			print('level_step,zRange = '+str(self.level_step)+','+str(self.zRange))

	def round_range(self):
		if self.PC.plot_2D:
			self.zRange[0] = MF.round_sig(self.zRange[0], 2)
			self.zRange[1] = MF.round_sig(self.zRange[1], 2)
			self.level_step = (self.zRange[1]-self.zRange[0])/self.nlevels
			self.level_step = MF.round_sig(self.level_step, 2)

	def set_font_size(self,font_size):
		self.font_size = font_size

	def set_lim(self,define_lim,xlim,ylim,zlim = None):
		self.define_lim = define_lim
		self.xlim = xlim
		self.ylim = ylim
		self.zlim = zlim

	def set_legend(self,define_legend,legend):
		self.define_legend = define_legend
		self.legend = legend

	def plot_contour(self,X,Y,Z,levels,xlabel,ylabel,range_set):
		if (range_set):
			fig = plt.contourf(X,Y,Z,levels=levels)
			print('plotting with range set')
		else:
			fig = plt.contourf(X,Y,Z)
			print('plotting without range set')
		plt.xlabel(xlabel)
		plt.ylabel(ylabel)
		return fig

	def plot_flexible(self,transpose):
		if transpose:
			fig = plt.plot(self.x,self.y,label=self.legend)
			plt.xlabel(self.xlabel)
			plt.ylabel(self.ylabel)
			if (self.range_set):
				plt.xlim(self.xRange)
				plt.ylim(self.yRange)
		else:
			fig = plt.plot(self.y,self.x,label=self.legend)
			if (self.range_set):
				plt.xlim(self.yRange)
				plt.ylim(self.xRange)
			plt.xlabel(self.ylabel)
			plt.ylabel(self.xlabel)
		return fig

	def plot(self):
		plt.figure()
		plt.rc('font', **self.font)
		leg = IO.get_file_from_path(self.target_file_name)
		self.set_legend(True,leg)
		if self.PC.plot_1D:
			print('Plotting 1D')
			fig = self.plot_flexible(True)
		elif self.PC.plot_2D:
			print('Plotting 2D')
			levels = np.arange(self.zRange[0],self.zRange[1],self.level_step)
			if self.PC.direction_2D==3:
				fig = self.plot_contour(self.Y,self.X,self.Z,levels,self.ylabel,self.xlabel,self.range_set)
			else:
				fig = self.plot_contour(self.X,self.Y,self.Z,levels,self.xlabel,self.ylabel,self.range_set)
			cbar = plt.colorbar(fig)
			# cbar.ax.set_ylabel(self.zlabel)
			plt.title(self.zlabel)
			cbar.ax.set_ylabel('')
			# if (self.zlabel): plt.zlabel(self.zlabel)
			if (self.range_set): plt.xlim(self.xRange)
			if (self.range_set): plt.ylim(self.yRange)
		plt.legend([self.legend])
		pylab.savefig(self.target_file+'.png', bbox_inches='tight')
		print('Just saved '+self.target_file+'.png')
		# plt.show()
		# plt.show(block=False)
		# plt.draw()
		return fig

	def close_plots(self):
		plt.cla()
		plt.clf()
		plt.close()
