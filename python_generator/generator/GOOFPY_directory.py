import funcs as f
class GOOFPY_directory:
	def __init__(self,PS):
		self.PS = PS
		self.GOOFPY_dir               = ''
		self.target_root              = ''
		self.target_dir               = ''
		self.main                     = ''
		self.bin_dir                  = ''
		self.base_dir                 = ''
		self.makefile_dir             = ''
		self.makefile_file            = ''
		self.interface_dir = ''
		self.common_root              = ''
		self.fext = '.f90'
		return

	def set_target_root(self,target_root): self.target_root = target_root
	def set_interface_dir(self,interface_dir): self.interface_dir = interface_dir
	def set_target_dir(self,target_dir): self.target_dir = target_dir
	def set_GOOFPY_dir(self,GOOFPY_dir): self.GOOFPY_dir = GOOFPY_dir
	def set_makefile_dir(self,makefile_dir): self.makefile_dir = makefile_dir
	def set_makefile_file(self,makefile_file): self.makefile_file = makefile_file
	def set_main(self,main): self.main = main
	def set_bin_dir(self,bin_dir): self.bin_dir = bin_dir
	def set_base_dir(self,base_dir): self.base_dir = base_dir

	def compute_common_root(self):
		self.dir_list = []
		self.dir_list = self.dir_list+[self.GOOFPY_dir]
		self.dir_list = self.dir_list+[self.target_root]
		self.dir_list = self.dir_list+[self.target_dir]
		self.dir_list = self.dir_list+[self.main]
		self.dir_list = self.dir_list+[self.bin_dir]
		self.dir_list = self.dir_list+[self.base_dir]
		self.dir_list = self.dir_list+[self.interface_dir]
		self.dir_list = self.dir_list+[self.makefile_dir]
		self.dir_list = self.dir_list+[self.makefile_file]
		temp = self.dir_list
		temp = [x for x in temp if x]
		temp = [x for x in temp if not x=='']
		self.common_root = temp[0]
		for x in temp[1:]:
			self.common_root = f.longest_substring_finder(self.common_root, x)

	def set_dir(self,GOOFPY_dir,target_root,main,interface_dir):
		PS = self.PS
		self.GOOFPY_dir = GOOFPY_dir+PS
		self.target_root = target_root+PS
		self.target_dir = target_root+PS+'generated_code'+PS
		self.main = main
		self.bin_dir = target_root+PS+'bin'+PS
		self.base_dir = GOOFPY_dir+'base'+PS
		self.interface_dir = interface_dir
		self.compute_common_root()
		self.fext = '.f90'
		return self

	def print_local(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS                       = '+self.PS)
		print('GOOFPY_dir               = '+self.GOOFPY_dir.replace(self.common_root,''))
		print('target_root              = '+self.target_root.replace(self.common_root,''))
		print('target_dir               = '+self.target_dir.replace(self.common_root,''))
		print('base_dir                 = '+self.base_dir.replace(self.common_root,''))
		print('bin                      = '+self.bin_dir.replace(self.common_root,''))
		print('main                     = '+self.main.replace(self.common_root,''))
		print('interface_dir            = '+self.interface_dir.replace(self.common_root,''))
		print('makefile_dir             = '+self.makefile_dir.replace(self.common_root,''))
		print('makefile_file            = '+self.makefile_file.replace(self.common_root,''))
		print('common_root              = '+self.common_root)
		print('fext                     = '+self.fext)
		print('--------------------------------------')

	def print(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS                       = '+self.PS)
		print('GOOFPY_dir               = '+self.GOOFPY_dir)
		print('target_root              = '+self.target_root)
		print('target_dir               = '+self.target_dir)
		print('base_dir                 = '+self.base_dir)
		print('bin                      = '+self.bin_dir)
		print('main                     = '+self.main)
		print('interface_dir            = '+self.interface_dir)
		print('makefile_dir             = '+self.makefile_dir)
		print('makefile_file            = '+self.makefile_file)
		print('common_root              = '+self.common_root)
		print('fext                     = '+self.fext)
		print('--------------------------------------')

