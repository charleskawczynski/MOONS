import funcs as f
class GOOFPY_directory:
	def __init__(self,PS):
		self.PS = PS
		self.dir_GOOFPY               = ''
		self.dir_MOONS                = ''
		self.dir_code                 = ''
		self.dir_app                  = ''
		self.dir_code_pre_generated   = ''
		self.dir_code_generated       = ''
		self.dir_code_handwritten     = ''
		self.file_main                = ''
		self.dir_bin                  = ''
		self.dir_makefile             = ''
		self.makefile_main            = ''
		self.src_generated            = ''
		self.src_pre_generated        = ''
		self.src_handwritten          = ''
		self.src_all                  = ''
		self.vpath_pre_generated      = ''
		self.vpath_handwritten        = ''
		self.vpath_generated          = ''
		self.vpath_all                = ''
		self.make_file_vpaths         = ''
		self.PS_make_file             = ''
		self.PS_file_sys              = ''
		self.dir_interface            = ''
		self.common_root              = ''
		self.f_ext                    = ''
		return

	def set_dir_GOOFPY(self,dir_GOOFPY): self.dir_GOOFPY = dir_GOOFPY
	def set_dir_MOONS(self,dir_MOONS): self.dir_MOONS = dir_MOONS
	def set_dir_app(self,dir_app): self.dir_app = dir_app
	def set_f_ext(self,f_ext): self.f_ext = f_ext
	def set_dir_interface(self,dir_interface): self.dir_interface = dir_interface
	def set_dir_code_generated(self,dir_code_generated): self.dir_code_generated = dir_code_generated
	def set_dir_code_pre_generated(self,dir_code_pre_generated): self.dir_code_pre_generated = dir_code_pre_generated
	def set_dir_code_handwritten(self,dir_code_handwritten): self.dir_code_handwritten = dir_code_handwritten
	def set_dir_code(self,dir_code): self.dir_code = dir_code
	def set_dir_makefile(self,dir_makefile): self.dir_makefile = dir_makefile
	def set_makefile_main(self,makefile_main): self.makefile_main = makefile_main
	def set_src_generated(self,src_generated): self.src_generated = src_generated
	def set_src_pre_generated(self,src_pre_generated): self.src_pre_generated = src_pre_generated
	def set_src_handwritten(self,src_handwritten): self.src_handwritten = src_handwritten
	def set_src_all(self,src_all): self.src_all = src_all
	def set_vpath_pre_generated(self,vpath_pre_generated): self.vpath_pre_generated = vpath_pre_generated
	def set_vpath_handwritten(self,vpath_handwritten): self.vpath_handwritten = vpath_handwritten
	def set_vpath_generated(self,vpath_generated): self.vpath_generated = vpath_generated
	def set_vpath_all(self,vpath_all): self.vpath_all = vpath_all
	def set_make_file_vpaths(self,make_file_vpaths): self.make_file_vpaths = make_file_vpaths
	def set_PS_make_file(self,PS_make_file): self.PS_make_file = PS_make_file
	def set_PS_file_sys(self,PS_file_sys): self.PS_file_sys = PS_file_sys
	def set_file_main(self,file_main): self.file_main = file_main
	def set_dir_bin(self,dir_bin): self.dir_bin = dir_bin

	def compute_common_root(self):
		self.dir_list = []
		self.dir_list = self.dir_list+[self.dir_GOOFPY]
		self.dir_list = self.dir_list+[self.dir_MOONS]
		self.dir_list = self.dir_list+[self.dir_code]
		self.dir_list = self.dir_list+[self.dir_app]
		self.dir_list = self.dir_list+[self.dir_code_generated]
		self.dir_list = self.dir_list+[self.dir_code_pre_generated]
		self.dir_list = self.dir_list+[self.dir_code_handwritten]
		self.dir_list = self.dir_list+[self.file_main]
		self.dir_list = self.dir_list+[self.dir_bin]
		self.dir_list = self.dir_list+[self.dir_interface]
		self.dir_list = self.dir_list+[self.dir_makefile]
		self.dir_list = self.dir_list+[self.makefile_main]
		self.dir_list = self.dir_list+[self.src_generated]
		self.dir_list = self.dir_list+[self.src_pre_generated]
		self.dir_list = self.dir_list+[self.src_handwritten]
		self.dir_list = self.dir_list+[self.src_all]
		self.dir_list = self.dir_list+[self.vpath_pre_generated]
		self.dir_list = self.dir_list+[self.vpath_handwritten]
		self.dir_list = self.dir_list+[self.vpath_generated]
		self.dir_list = self.dir_list+[self.vpath_all]
		temp = self.dir_list
		temp = [x for x in temp if x]
		temp = [x for x in temp if not x=='']
		self.common_root = temp[0]
		for x in temp[1:]:
			self.common_root = f.longest_substring_finder_new(self.common_root, x)

	def unify_path_separator(self):
		self.dir_GOOFPY             = self.dir_GOOFPY.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_MOONS              = self.dir_MOONS.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_code               = self.dir_code.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_app                = self.dir_app.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_code_generated     = self.dir_code_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_code_pre_generated = self.dir_code_pre_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_code_handwritten   = self.dir_code_handwritten.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_bin                = self.dir_bin.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.file_main              = self.file_main.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_interface          = self.dir_interface.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.dir_makefile           = self.dir_makefile.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.makefile_main          = self.makefile_main.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.src_generated          = self.src_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.src_pre_generated      = self.src_pre_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.src_handwritten        = self.src_handwritten.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.src_all                = self.src_all.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.vpath_pre_generated    = self.vpath_pre_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.vpath_handwritten      = self.vpath_handwritten.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.vpath_generated        = self.vpath_generated.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.vpath_all              = self.vpath_all.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		self.common_root            = self.common_root.replace('/',self.PS_file_sys).replace('\\',self.PS_file_sys)
		return self

	def print_local(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS                       = '+self.PS)
		print('dir_GOOFPY               = '+self.dir_GOOFPY.replace(self.common_root,''))
		print('dir_MOONS                = '+self.dir_MOONS.replace(self.common_root,''))
		print('dir_code                 = '+self.dir_code.replace(self.common_root,''))
		print('dir_code                 = '+self.dir_code.replace(self.common_root,''))
		print('dir_app                  = '+self.dir_app.replace(self.common_root,''))
		print('dir_code_generated       = '+self.dir_code_generated.replace(self.common_root,''))
		print('dir_code_pre_generated   = '+self.dir_code_pre_generated.replace(self.common_root,''))
		print('dir_code_handwritten     = '+self.dir_code_handwritten.replace(self.common_root,''))
		print('bin                      = '+self.dir_bin.replace(self.common_root,''))
		print('file_main                = '+self.file_main.replace(self.common_root,''))
		print('dir_interface            = '+self.dir_interface.replace(self.common_root,''))
		print('dir_makefile             = '+self.dir_makefile.replace(self.common_root,''))
		print('makefile_main            = '+self.makefile_main.replace(self.common_root,''))
		print('src_generated            = '+self.src_generated.replace(self.common_root,''))
		print('src_pre_generated        = '+self.src_pre_generated.replace(self.common_root,''))
		print('src_handwritten          = '+self.src_handwritten.replace(self.common_root,''))
		print('src_all                  = '+self.src_all.replace(self.common_root,''))
		print('vpath_pre_generated      = '+self.vpath_pre_generated.replace(self.common_root,''))
		print('vpath_handwritten        = '+self.vpath_handwritten.replace(self.common_root,''))
		print('vpath_generated          = '+self.vpath_generated.replace(self.common_root,''))
		print('vpath_all                = '+self.vpath_all.replace(self.common_root,''))
		print('common_root              = '+self.common_root)
		print('f_ext                    = '+self.f_ext)
		print('--------------------------------------')

	def print(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS                       = '+self.PS)
		print('PS_file_sys              = '+self.PS_file_sys)
		print('PS_make_file             = '+self.PS_make_file)
		print('dir_GOOFPY               = '+self.dir_GOOFPY)
		print('dir_MOONS                = '+self.dir_MOONS)
		print('dir_code                 = '+self.dir_code)
		print('dir_app                  = '+self.dir_app)
		print('dir_code_generated       = '+self.dir_code_generated)
		print('dir_code_pre_generated   = '+self.dir_code_pre_generated)
		print('dir_code_handwritten     = '+self.dir_code_handwritten)
		print('bin                      = '+self.dir_bin)
		print('file_main                = '+self.file_main)
		print('dir_interface            = '+self.dir_interface)
		print('dir_makefile             = '+self.dir_makefile)
		print('makefile_main            = '+self.makefile_main)
		print('src_generated            = '+self.src_generated)
		print('src_pre_generated        = '+self.src_pre_generated)
		print('src_handwritten          = '+self.src_handwritten)
		print('src_all                  = '+self.src_all)
		print('vpath_pre_generated      = '+self.vpath_pre_generated)
		print('vpath_handwritten        = '+self.vpath_handwritten)
		print('vpath_generated          = '+self.vpath_generated)
		print('vpath_all                = '+self.vpath_all)
		print('common_root              = '+self.common_root)
		print('f_ext                    = '+self.f_ext)
		print('--------------------------------------')

