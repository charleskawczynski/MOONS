import matplotlib.pyplot as plt
import file_IO as IO
import pylab
class direction:
	def __init__(self):
		return

	def set_header(self,header):
		self.header = header
		self.S = IO.get_shape(self.header)
		self.S_2D = tuple([k for k in self.S if k])
		R = range(1,4)
		self.direction_2D = tuple([i for i,k in zip(R,self.S) if not k])[0]
		print('self.S = '+str(self.S))
		print('self.S_2D = '+str(self.S_2D))
		print('self.direction_2D = '+str(self.direction_2D))
		print('self.data.shape = '+str(self.data.shape))
		self.x_dir = direction()
