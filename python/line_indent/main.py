import sys
import os
import file_IO as IO
clear = lambda: os.system('cls')
clear()

PS = '\\'
up = '..'
make_dir = '.'+PS+up+PS+up+PS+'makefiles'+PS
code_dir = '.'+PS+up+PS+up+PS+'code'
# print code_dir
# print ' ----------------------------------------------- '
print ' ----------------------------------------------- '
print ''
print 'Number of compiled lines:'
print ''

files = IO.get_all_files_in_path(make_dir)
make_file = make_dir+files[0]
f = IO.get_file_contents(make_file)
compiled_files = filter(None, f.split('\n'))


compiled_files = [x[:-1] for x in compiled_files if x.endswith('.f90\\')]
# print '\n'.join(compiled_files)
# print ' ----------------------------------------------- '

compiled_files = [x.replace('	$(SRC_DIR)',code_dir) for x in compiled_files]
compiled_files = [x.replace('$(PS)',PS) for x in compiled_files]
# print '\n'.join(compiled_files)

n_lines_total = 0
for f in compiled_files:
	n_lines_file = IO.file_len(f)
	print str(n_lines_file)+ '		' + f
	n_lines_total = n_lines_total + n_lines_file

print ''
print 'Total number of compiled lines = '+str(n_lines_total)
print 'Total number of files = '+str(len(compiled_files))


IO.delete_pyc_files()
