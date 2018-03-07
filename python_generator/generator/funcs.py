import GOOFPY_directory as GD
import file_IO as IO
import os
import copy
import sys
import glob
from collections import OrderedDict
import sorting as SORT
import sorting_backup as SORT_BU
import re

def flatten(foo):
    l = []
    for x in foo:
        if hasattr(x, '__iter__') and not isinstance(x, str):
            for y in flatten(x):
                l.append(y)
        else:
            l.append(x)
    return l

def make_dummy_main(file_name,class_list,base_spaces):
    L =  ['program main']
    for x in class_list: L=L+['use '+x+'_mod']
    L =L+['implicit none']
    L =L+['write(*,*) "success!"']
    L =L+['end program']
    L = [base_spaces+x for x in L]
    IO.write_list_to_file(file_name,L)

# MAKE BATCH FILE***********************************

def make_dot_bat(d,dir_app,source_dir,generated_path,class_list,dir_base,base_files,PS):
    L = ['gfortran -fopenmp -g -cpp -fimplicit-none -Wall -Wextra ']
    L = L + ['-Wall -Wextra -pedantic -fbacktrace -fcheck=all -Wuninitialized ']
    L = L + ['-J"bin" ']
    prefix = ''.join(L)
    L = []
    file_list = [generated_path+x+'.f90' for x in class_list]
    sorted_file_list = get_sorted_file_list_recent(d,file_list)

    L = [dir_base+x+' ' for x in base_files]
    L = ['..'+PS+'source'+PS+dir_base+x+' ' for x in base_files]

    L = L + sorted_file_list

    L = [x.replace(source_dir,'')+' ' for x in L]
    L = [x.replace(dir_app,'')+' ' for x in L]
    object_files = [x.replace('.f90','.o') for x in L]
    object_files_s = ''.join(object_files)
    L = ['-c -o '+ x.replace('.f90','.o')+ x + '\n' for x in L]
    L = [prefix + x for x in L]
    L = L + [prefix+' -o output '+generated_path.replace(dir_app,'')+'main_dummy.f90 '+object_files_s]
    L = L + ['\n output.exe']
    L = L + ['\n cd generated_code']
    L = L + ['\n del *.o']
    L = L + ['\n cd ..']
    L = [''.join(L)]
    IO.write_list_to_file(dir_app+'run.bat',L)
    return

def make_makefile(d,makefile_file,vpath_file,file_list,paths_to_replace,PS):
    L = []
    L = L + ['$(SRC_DIR_GENERATED)$(PS)']
    prefix = ''.join(L)
    sorted_file_list = get_sorted_file_list_recent(d,file_list)
    L_start = sorted_file_list

    # Path
    L = L_start
    for y in paths_to_replace:
        L = [x.replace(y,'')+' ' for x in L]
    L = [x.replace('/','$(PS)')+' ' for x in L]
    L = [prefix + x for x in L]
    L = ['\t'+x for x in L]
    L = [x.replace(' ','') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = ['$(PS)'.join(x.split("$(PS)")[0:-1]) if "$(PS)" in x else x for x in L]
    L = list(set(L))
    L.sort()
    L = ['VPATH +='] + L
    L = [x+'\\\n' for x in L]
    L = L+['\n']
    L_VPATH = L
    IO.write_list_to_file(vpath_file,[''.join(L_VPATH)])

    # Files
    L = L_start
    for y in paths_to_replace:
        L = [x.replace(y,'')+' ' for x in L]
    L = [x.replace('/','$(PS)')+' ' for x in L]
    L = [prefix + x for x in L]
    L = ['\t'+x for x in L]
    L = ['SRCS_F +=\\'] + L
    L = [x.replace(' ','') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = [x+'\n' for x in L]
    L_FILES = L

    IO.write_list_to_file(makefile_file,[''.join(L_FILES)])
    return

def make_generated(d,file_list):
    L = []
    PS = d.PS_make_file
    L = L + ['$(SRC_DIR_GENERATED)'+PS]
    prefix = ''.join(L)
    sorted_file_list = get_sorted_file_list_recent(d,file_list)
    L_start = sorted_file_list
    paths_to_replace = [d.dir_app,d.dir_GOOFPY,d.dir_code_generated]
    vpaths_files = [d.vpath_pre_generated,d.vpath_handwritten,d.vpath_generated]

    # Path
    L = L_start
    for y in paths_to_replace:
        L = [x.replace(y,'')+' ' for x in L]
    L = [x.replace('/',PS)+' ' for x in L]
    L = [prefix + x for x in L]
    L = ['\t'+x for x in L]
    L = [x.replace(' ','') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = [PS.join(x.split(PS)[0:-1]) if PS in x else x for x in L]
    L = list(set(L))
    L.sort()
    L = ['VPATH +='] + L
    L = [x+'\\\n' for x in L]
    L = L+['\n']
    L_VPATH = L
    IO.write_list_to_file(d.vpath_generated,[''.join(L_VPATH)])

    # Files
    L = L_start
    for y in paths_to_replace:
        L = [x.replace(y,'')+' ' for x in L]
    L = [x.replace('/',PS)+' ' for x in L]
    L = [prefix + x for x in L]
    L = ['\t'+x for x in L]
    L = ['SRCS_F +=\\'] + L
    L = [x.replace(' ','') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = [x+'\n' for x in L]
    L_FILES = L

    IO.write_list_to_file(d.src_generated,[''.join(L_FILES)])
    return

def get_defined_vars(d):
    L = IO.read_file_to_list(d,'get_defined_vars')
    L = [x for x in L if '=' in x]
    # L = [x for x in L if "'" in x]
    L = [x for x in L if not x.startswith('#')]
    L = [x for x in L if not x.startswith('\t')]
    L = [x for x in L if not '+' in x]
    L = [x for x in L if not '$' in x]
    L = [(x.split('=')[0].strip(),x.split('=')[1].strip()) for x in L]
    L = [(x,y.replace("'",'')) for x,y in L]
    defined_vars = L
    return defined_vars

def get_compiled_files(d,make_src_files):
    f = ''.join([IO.get_file_contents(x) for x in make_src_files])
    compiled_files = list(filter(None, f.split('\n')))
    compiled_files = [x for x in compiled_files if x.endswith('.f90\\') or x.endswith('.f90')]
    compiled_files = [x[:-1] if x.endswith('\\') else x for x in compiled_files]
    compiled_files = [x.replace('\t','') for x in compiled_files]
    compiled_files = [x for x in compiled_files if not x.startswith('#')]
    root_paths = [x.split(d.PS_make_file)[0] for x in compiled_files]
    root_paths = list(set(root_paths))
    root_paths = [x for x in root_paths if not '=' in x]
    root_paths = [x.replace('(','') for x in root_paths]
    root_paths = [x.replace(')','') for x in root_paths]
    root_paths = [x.replace('$','') for x in root_paths]
    make_file_main = IO.get_file_contents(d.makefile_main)
    make_file_main = list(filter(None, make_file_main.split('\n')))
    root_path_replace = [x for x in make_file_main if any([x.startswith(y) for y in root_paths])]
    root_path_replace = [('$('+x.split('=')[0].replace(' ','')+')',x.split('=')[1].replace(' ','')) for x in root_path_replace]
    compiled_files = [x.replace(y,z) for x in compiled_files for y,z in root_path_replace if y in x]
    compiled_files = [x.replace(d.PS_make_file,d.PS_file_sys) for x in compiled_files]
    compiled_files = [d.dir_MOONS+x for x in compiled_files]
    compiled_files = [x.replace('..','') for x in compiled_files]
    compiled_files = [x.replace(d.PS_file_sys+d.PS_file_sys,d.PS_file_sys) for x in compiled_files]
    defined_vars = get_defined_vars(d.makefile_main)
    for DV in defined_vars:
        compiled_files = [x.replace('$('+DV[0]+')',DV[1]) for x in compiled_files]
    root_path_replace = [(x,y.replace('..','')) for x,y in root_path_replace]
    root_path_replace = [(x,y.replace(d.PS_make_file,d.PS_file_sys)) for x,y in root_path_replace]
    return (compiled_files,root_path_replace,defined_vars)

def combine_makefile(d):
    L = []
    make_src_files = [d.src_generated,d.src_pre_generated,d.src_handwritten]
    (compiled_files,root_path_replace,defined_vars) = get_compiled_files(d,make_src_files)
    compiled_files = list(set(compiled_files))
    sorted_file_list = get_sorted_file_list(d,compiled_files)
    defined_vars = [(x,y) for x,y in defined_vars if not 'rm' == x.lower()]
    defined_vars = [(x,y) for x,y in defined_vars if not 'rm' == y.lower()]
    L = sorted_file_list
    for MFV in d.make_file_vpaths:
        L = [x.replace(MFV[0],MFV[1]) for x in L]
    L = [x.replace(d.PS_file_sys,d.PS_make_file) for x in L]
    for DV in defined_vars:
        L = [x.replace(DV[1],'$('+DV[0]+')') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = ['\t'+x for x in L]
    L = [x.replace(' ','') for x in L]
    L = ['SRCS_F +=\\'] + L
    L = [x+'\n' for x in L]

    IO.write_list_to_file(d.src_all,[''.join(L)])
    return

def get_sorted_file_list(d,file_list):
    L = []
    module_names = get_list_of_module_names(file_list)
    sorted_module_list = SORT.sort_files_by_dependency(file_list,d.PS_file_sys)
    sorted_file_list = get_file_list_from_module_names(d,file_list,sorted_module_list)
    return sorted_file_list

def get_sorted_file_list_recent(d,file_list):
    L = []
    module_names = get_list_of_module_names(file_list)
    sorted_module_list = SORT_BU.sort_files_by_dependency_recent(file_list,module_names)
    sorted_file_list = get_file_list_from_module_names(d,file_list,sorted_module_list)
    return sorted_file_list

def get_list_of_files_in_dir(path,ext):
    return [f.replace(ext,'') for f in os.listdir(path) if f.endswith(ext)]

def get_list_of_module_names(file_list):
    d = OrderedDict()
    for f in file_list:
        d[f] = flatten(get_module_name(f))
    return flatten([d[key] for key in d])

def get_module_name(f):
    L = IO.read_file_to_list(f,'get_module_name')
    L = [x.lstrip() for x in L]
    L = [x.rstrip() for x in L]
    L = [x for x in L if not x.startswith('!')]
    L = [x for x in L if not x.startswith('C')]
    L = [x for x in L if x.startswith('module ')]
    L = [x.replace('module ','') for x in L]
    return L

def get_file_list_from_module_names(d,file_list,module_names):
    L = []
    L_added = []
    for m in module_names:
        for f in file_list:
            m_name = m.replace('_mod','')
            f_name = f.split(d.PS_file_sys)[-1]
            f_name = f_name.replace(d.f_ext,'').replace(d.f_ext,'').replace('_mod','')
            duplicate = any([f_name==x for x in L_added])
            if f_name==m_name and not duplicate:
                L=L+[f]
                L_added=L_added+[f_name]
            elif f_name in m_name or m_name in f_name:
                pass
    return L

def get_main_program(path,FL):
    f_ext = '.f90'
    program_files = []
    for f in FL:
        L = IO.read_file_to_list(path + f + f_ext,'get_main_program')
        L = [x.lstrip() for x in L]
        L = [x.rstrip() for x in L]
        L = [x for x in L if not x.startswith('!')]
        L = [x for x in L if not x.startswith('C')]
        L = [x.startswith('program') for x in L]
        if L: program_files.append(f)
    return program_files

def longest_substring_finder(string1, string2):
    answer = ""
    len1, len2 = len(string1), len(string2)
    for i in range(len1):
        match = ""
        for j in range(len2):
            if (i + j < len1 and string1[i + j] == string2[j]):
                match += string2[j]
            else:
                if (len(match) > len(answer)): answer = match
                match = ""
    return answer

def longest_substring_finder_new(string1, string2):
    len_min = min([len(string1), len(string2)])
    answer = ""
    for i in range(len_min):
        if string1[i] == string2[i]:
            answer += string1[i]
        else:
            break
    return answer
