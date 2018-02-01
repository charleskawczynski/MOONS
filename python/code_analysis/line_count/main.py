import sys
import os
import file_IO as IO
from os import listdir
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

PS = '\\'
up = '..'+PS
generated = False

make_dir = '.'+PS+up+up+up+'makefiles'+PS
# make_file_name = ['handwritten.make']
# make_file_name = ['pre_generated.make']
# make_file_name = ['pre_generated.make']
# make_file_name = ['generated.make']

# make_file_name = ['pre_generated.make','handwritten.make']
make_file_name = ['pre_generated.make','generated.make','handwritten.make']

code_dir = '.'+PS+up+up+up+'code'+PS
code_dir_generated = code_dir+'generated_code'
code_dir_handwritten = code_dir+'handwritten'
code_dir_pre_generated = code_dir+'pre_generated'
exceptions = code_dir_handwritten+PS+'abstract_interfaces'
# print(code_dir)
# print(' ----------------------------------------------- ')
print('make_dir = '+make_dir)
print('code_dir = '+code_dir)
print('code_dir_generated = '+code_dir_generated)
print('code_dir_handwritten = '+code_dir_handwritten)
print('code_dir_pre_generated = '+code_dir_pre_generated)
print(' ----------------------------------------------- ')
print('')
print('Number of compiled lines:')
print('')

files = IO.get_all_files_in_path(make_dir)
make_files = [x for x in files if any([y in x for y in make_file_name])]
make_files = [make_dir+x for x in make_files]
print('makefiles = ')
print('\n'.join(make_files))
print('')

f = IO.get_file_contents(make_files[0])
fs = [IO.get_file_contents(x) for x in make_files]
f = ''.join(fs)
compiled_files = filter(None, f.split('\n'))
compiled_files = [x for x in compiled_files if x.endswith('.f90\\') or x.endswith('.f90') and '$(SRC_DIR)' in x]
compiled_files = [x[:-1] if x.endswith('\\') else x for x in compiled_files]

# print('\n'.join(compiled_files))
# print(' ----------------------------------------------- ')

# compiled_files = [x for x in compiled_files if '$(SRC_DIR_GENERATED)' in x]

compiled_files = [x.replace('	$(SRC_DIR_PRE_GENERATED)',code_dir_pre_generated) for x in compiled_files]
compiled_files = [x.replace('	$(SRC_DIR_GENERATED)',code_dir_generated) for x in compiled_files]
compiled_files = [x.replace('	$(SRC_DIR)',code_dir_handwritten) for x in compiled_files]
# if generated:
# else:
compiled_files = [x.replace('$(PS)',PS) for x in compiled_files]
compiled_files = [x.replace('$(sim_params_default)','sim_params_default_Bandaru') for x in compiled_files]
compiled_files = [x for x in compiled_files if not x.replace('\t','').startswith('#')]
print('\n'.join(compiled_files))

n_lines_total = 0
# print('------------------------------------------- start file line count')
for f in compiled_files:
	n_lines_file = IO.file_len(f)
	f_short = f.replace(code_dir,'')
	print(str(n_lines_file)+ '		' + f_short)
	n_lines_total = n_lines_total + n_lines_file
	# print('\n')
# print('------------------------------------------- end   file line count')

print('')
print('Total number of compiled lines = '+str(n_lines_total))
print('Total number of files = '+str(len(compiled_files)))

# onlyfiles = [f for f in listdir(code_dir) if isfile(join(code_dir, f))]
# print('\n'.join(onlyfiles))

all_files_in_code = []
for path, subdirs, files in os.walk(code_dir):
	for name in files:
		all_files_in_code.append(os.path.join(path, name))


non_compiled_files = list(set(all_files_in_code) - set(compiled_files))
# non_compiled_files = [x for x in non_compiled_files if not any([y in x for y in exceptions])]
non_compiled_files.sort()

# print(' ---------------- ALL FILES -------------------- ')
# print('\n'.join(all_files_in_code))
# print(' ---------------- COMPILED FILES -------------------- ')
# print('\n'.join(compiled_files))
print('')
print(' ---------------- NOT COMPILED FILES -------------------- ')
print('\n'.join(non_compiled_files))
print('')
print('Number of files not compiled = '+str(len(non_compiled_files)))

IO.delete_pyc_files()
