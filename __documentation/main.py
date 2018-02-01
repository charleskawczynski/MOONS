import os
clear = lambda: os.system('cls')
clear()
import subprocess

def delete_auxiliary_files(file_name,extensions):
	print('-------------------------------------')
	for e in extensions:
		f = ''.join(file_name.split('.')[0:-1])+e
		print('about to remove file '+f)
		if os.path.isfile(f): os.remove(f)
	print('-------------------------------------')

def get_file_contents(file_name):
	with open(file_name, 'r') as content_file: contents = content_file.read()
	return contents.split('\n')

def write_list_to_file(file_name,L):
	f = open(file_name,'w+')
	f.write('\n'.join(L))
	f.close()

def read_file_to_list(file_name):
	with open(file_name,encoding='utf8') as f: # Accounts for encoded characters
		L = f.read().splitlines() # list of lines
	return L

def clean_up_trash(file_name):
	delete_auxiliary_files(file_name,['.aux','.txt','.out','.log','.blg','.gz','.synctex.gz'])

def build_latex_file(file_name):
	# This needs to be done locally somehow.
	# These files that python tries to build
	# are built in the root directory. Maybe
	# find a "cd" command?

	# subprocess.check_call(['pdflatex', file_name])
	# subprocess.check_call(['xelatex', file_name])
	clean_up_trash(file_name)

def main_run():
	p = 'C:/Users/Charlie/Documents/GitHub/MOONS/__documentation'
	prepend_PS_chain_content = ['\edef\PSCHAIN{../\PSCHAIN}']
	prepend_PS_chain_content += ['\input{\PSCHAIN/prepend_PS_chain}']
	prepend_PS_chain_file = 'prepend_PS_chain.tex'

	s_old = '\input{../includes.tex}'
	s_bib_old = '\input{../include_bib.tex}'
	s_bib_new = '\input{\\rootdir/includes/include_bib.tex}'
	preamble  = ['\\newcommand{\\PSCHAIN}{..}\n']
	preamble += ['\\input{../prepend_PS_chain}\n']
	preamble += ['\\newcommand{\\rootdir}{\\PSCHAIN}\n']
	preamble += ['\\input{\PSCHAIN/includes/includes}\n']
	# print(prepend_PS_chain_content)
	PS = '/'
	n_files = 0
	for subdir, dirs, files in os.walk(p):
		tex_files = [x for x in files if x.endswith('.tex')]
		# tex_files = [x for x in tex_files if prepend_PS_chain_file == x]
		tex_files = [x for x in tex_files if not prepend_PS_chain_file == x]
		# print(tex_files)
		tex_files = [x for x in tex_files if not 'includes.tex' == x]
		tex_files = [x for x in tex_files if not 'include_bib.tex' == x]
		tex_files = [x for x in tex_files if not 'include_zero_margins.tex' == x]
		tex_files = [x for x in tex_files if not 'LATEX_INCLUDES' in subdir]
		for x in tex_files:
			full_dir = subdir.replace('\\','/')+PS+x.replace('\\','/')
			n_files+=1
			# if 'LU_decomp' in full_dir and 'analysis' in full_dir:
			if True:
				print(full_dir[40:])
				build_latex_file(full_dir)

			if False:
				L = read_file_to_list(full_dir)
				L_new = L
				L_new = [x.replace(s_old,''.join(preamble)) for x in L_new]
				write_list_to_file(full_dir,L_new)

			if False:
				L = read_file_to_list(full_dir)
				L_new = L
				L_new = [x.replace(s_bib_old,s_bib_new) for x in L_new]
				write_list_to_file(full_dir,L_new)

	print('n_files = '+str(n_files))

main_run()