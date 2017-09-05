import sys
import os
import file_IO as IO
from os import listdir
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

PS = '\\'
up = '..'+PS
generated = True

make_dir = '.'+PS+up+up+up+'makefiles'+PS
if generated:
	make_file_name = 'generated.make'
	code_dir = '.'+PS+up+up+up+'code'+PS+'generated_code'
else:
	make_file_name = 'handwritten.make'
	code_dir = '.'+PS+up+up+up+'code'+PS+'handwritten'
# print(code_dir)
# print(' ----------------------------------------------- ')
print('make_dir = '+make_dir)
print('code_dir = '+code_dir)
print(' ----------------------------------------------- ')
print('')
print('Number of compiled lines:')
print('')

files = IO.get_all_files_in_path(make_dir)
make_file = [x for x in files if make_file_name in x]
make_file = make_dir+make_file[0]
print('makefile = '+make_file)
print('')

f = IO.get_file_contents(make_file)
compiled_files = filter(None, f.split('\n'))
compiled_files = [x for x in compiled_files if x.endswith('.f90\\') or x.endswith('.f90') and '$(SRC_DIR)' in x]
compiled_files = [x[:-1] if x.endswith('\\') else x for x in compiled_files]

# print('\n'.join(compiled_files))
# print(' ----------------------------------------------- ')

if generated:
	compiled_files = [x.replace('	$(SRC_DIR_GENERATED)',code_dir) for x in compiled_files]
else:
	compiled_files = [x.replace('	$(SRC_DIR)',code_dir) for x in compiled_files]
compiled_files = [x.replace('$(PS)',PS) for x in compiled_files]
# print('\n'.join(compiled_files))

n_lines_total = 0
# print('------------------------------------------- start file line count')
for f in compiled_files:
	n_lines_file = IO.file_len(f)
	print(str(n_lines_file)+ '		' + f.replace(code_dir,''))
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
