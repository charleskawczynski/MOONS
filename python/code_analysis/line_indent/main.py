import sys
import os
import file_IO as IO
import line_indent as LI
from os import listdir
from os.path import isfile, join
clear = lambda: os.system('cls')
clear()

PS = '\\'
up = '..'
make_dir = '.'+PS+up+PS+up+PS+'makefiles'+PS
code_dir = '.'+PS+up+PS+up+PS+'code'
print ' ----------------------------------------------- \n'
print 'Compiled files: \n'

files = IO.get_all_files_in_path(make_dir)
make_file = make_dir+files[0]
f = IO.get_file_contents(make_file)
compiled_files = filter(None, f.split('\n'))
compiled_files = [x for x in compiled_files if x.endswith('.f90\\') or x.endswith('.f90') and '$(SRC_DIR)' in x]
compiled_files = [x[:-1] if x.endswith('\\') else x for x in compiled_files]
compiled_files = [x.replace('	$(SRC_DIR)',code_dir) for x in compiled_files]
compiled_files = [x.replace('$(PS)',PS) for x in compiled_files]
# print '\n'.join(compiled_files)
compiled_files = compiled_files[0:2]
# print compiled_files
# print '\n'.join(compiled_files)

print ' ----------------------------------------------- \n'
print 'About to process files: \n'

for f in compiled_files:
	LI.process(f)

print ' ----------------- FINISHED -------------------- '


IO.delete_pyc_files()