import os
import copy
import sys
import shutil
import glob
from collections import OrderedDict
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

def read_file_to_list(file_name,caller):
    f = open(file_name,'r+')
    try:
        f = open(file_name,'r+')
        L = f.read().split('\n')
    except:
        L = []
        print(' ********* READ ERROR ********* '+caller)
        print(' ********* READ ERROR ********* '+caller)
        print(' ********* READ ERROR ********* '+caller)
    return L

def read_file_to_string(file_name):
    return '\n'.join(read_file_to_list(file_name,'read_file_to_string'))

def write_string_to_file(file_name,s):
    try:
        f = open(file_name,'w+')
        f.write(s)
        f.close()
    except:
        print(' ********* WRITE ERROR ********* write_string_to_list ')
        print(' ********* WRITE ERROR ********* write_string_to_list ')
        print(' ********* WRITE ERROR ********* write_string_to_list ')
    return

def write_list_to_file(file_name,L):
    f = open(file_name,'w+')
    f.write('\n'.join(L))
    f.close()
    return

def make_path(new_path):
    if not os.path.exists(new_path):
        os.makedirs(new_path)

def delete_entire_tree_safe(d):
    if os.path.exists(d):
        shutil.rmtree(d)

def make_dummy_main(file_name,class_list,base_spaces):
    L =  ['program main']
    for x in class_list: L=L+['use '+x+'_mod']
    L =L+['implicit none']
    L =L+['write(*,*) "success!"']
    L =L+['end program']
    L = [base_spaces+x for x in L]
    write_list_to_file(file_name,L)


# MAKE BATCH FILE***********************************

def make_dot_bat(target_root,source_dir,generated_path,class_list,base_dir,base_files,PS):
    L = ['gfortran -fopenmp -g -cpp -fimplicit-none -Wall -Wextra ']
    L = L + ['-Wall -Wextra -pedantic -fbacktrace -fcheck=all -Wuninitialized ']
    L = L + ['-J"bin" ']
    prefix = ''.join(L)
    L = []
    file_list = [generated_path+x+'.f90' for x in class_list]
    module_names = get_list_of_module_names(file_list)
    sorted_module_list = sort_files_by_dependency(file_list,module_names)
    sorted_file_list = get_file_list_from_module_names(file_list,sorted_module_list)

    L = [base_dir+x+' ' for x in base_files]
    L = ['..'+PS+'source'+PS+base_dir+x+' ' for x in base_files]

    L = L + sorted_file_list

    L = [x.replace(source_dir,'')+' ' for x in L]
    L = [x.replace(target_root,'')+' ' for x in L]
    # print('\n'.join(L))
    object_files = [x.replace('.f90','.o') for x in L]
    object_files_s = ''.join(object_files)
    L = ['-c -o '+ x.replace('.f90','.o')+ x + '\n' for x in L]
    L = [prefix + x for x in L]
    # print('\n'.join(L))
    L = L + [prefix+' -o output '+generated_path.replace(target_root,'')+'main_dummy.f90 '+object_files_s]
    L = L + ['\n output.exe']
    L = L + ['\n cd generated_code']
    L = L + ['\n del *.o']
    L = L + ['\n cd ..']
    # print('\n'.join(L))
    L = [''.join(L)]
    write_list_to_file(target_root+'run.bat',L)
    return

def make_makefile(makefile_file,target_root,source_dir,generated_path,class_list,base_dir,base_files,PS):
    L = []
    L = L + ['$(SRC_DIR_GENERATED)$(PS)']
    prefix = ''.join(L)
    # print('\n'.join(class_list))
    file_list = [generated_path+x+'.f90' for x in class_list]
    # print('\n'.join(file_list))
    module_names = get_list_of_module_names(file_list)
    # print('\n'.join(module_names))
    sorted_module_list = sort_files_by_dependency(file_list,module_names)
    # print('\n'.join(sorted_module_list))
    sorted_file_list = get_file_list_from_module_names(file_list,sorted_module_list)
    # print('\n'.join(sorted_file_list))
    # print('\n'.join(sorted_module_list))
    L = []
    # L = [base_dir+x+' ' for x in base_files]
    # L = ['..'+PS+'source'+PS+base_dir+x+' ' for x in base_files]
    L_start = L + sorted_file_list

    # Path
    L = L_start
    L = [x.replace(source_dir,'')+' ' for x in L]
    L = [x.replace(target_root,'')+' ' for x in L]
    L = [x.replace(generated_path,'')+' ' for x in L]
    # L = [x.lower() for x in L]
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

    # Files
    L = L_start
    L = [x.replace(source_dir,'')+' ' for x in L]
    L = [x.replace(target_root,'')+' ' for x in L]
    L = [x.replace(generated_path,'')+' ' for x in L]
    # L = [x.lower() for x in L]
    L = [x.replace('/','$(PS)')+' ' for x in L]
    L = [prefix + x for x in L]
    L = ['\t'+x for x in L]
    L = ['SRCS_F +=\\'] + L
    L = [x.replace(' ','') for x in L]
    L = [x+'\\' if x.endswith('.f90') else x for x in L]
    L = [x+'\n' for x in L]
    L_FILES = L

    L = [''.join(L_VPATH+L_FILES)]
    # L[-1] = L[-1][:-1]

    write_list_to_file(makefile_file,L)
    return

def get_list_of_files_in_dir(path,ext):
    return [f.replace(ext,'') for f in os.listdir(path) if f.endswith(ext)]

def get_list_of_module_names(file_list):
    d = OrderedDict()
    for f in file_list:
        d[f] = flatten(get_module_name(f))
    return flatten([d[key] for key in d])

def get_module_name(f):
    # print(f)
    L = read_file_to_list(f,'get_module_name')
    # L = [x.lower() for x in L]
    L = [x.lstrip() for x in L]
    L = [x.rstrip() for x in L]
    L = [x for x in L if not x.startswith('!')]
    L = [x for x in L if not x.startswith('C')]
    L = [x for x in L if x.startswith('module ')]
    return [x.replace('module ','') for x in L]

def get_file_list_from_module_names(file_list,module_names):
    file_list_sorted = []
    for m in module_names:
        for f in file_list:
            try:
                if m==get_module_name(f)[0]: file_list_sorted.append(f)
            except:
                pass
                # print('Error: potential name-mismatch')
    return file_list_sorted

def get_main_program(path,FL):
    fext = '.f90'
    program_files = []
    for f in FL:
        L = read_file_to_list(path + f + fext,'get_main_program')
        # L = [x.lower() for x in L]
        L = [x.lstrip() for x in L]
        L = [x.rstrip() for x in L]
        L = [x for x in L if not x.startswith('!')]
        L = [x for x in L if not x.startswith('C')]
        L = [x.startswith('program') for x in L]
        if L: program_files.append(f)
    return program_files

def sort_files_by_dependency(file_list,module_names):
    d = OrderedDict()
    SL = []
    for fn,mn in zip(file_list,module_names):
        L = read_file_to_list(fn,'sort_files_by_dependency')
        # L = [x.lower() for x in L]
        L = [x for x in L if not x.startswith('c')]
        L = [x.lstrip() for x in L]
        L = [x.rstrip() for x in L]
        L = [x for x in L if not x.startswith('!')]
        L = [x for x in L if x.startswith('use ')]
        L = [x.replace('use ','') for x in L]
        d[mn] = L
    return topological_sort(d)

def topological_sort(dict_to_be_sorted):
    # sort separately
    (sorted_dict,remaining) = separate_non_dependencies(dict_to_be_sorted)
    remaining_sorted = sort_remaining(remaining)
    for key in remaining_sorted:
        sorted_dict[key] = remaining_sorted[key]
    return sorted_dict

def separate_non_dependencies(dict_to_be_sorted):
    d = dict_to_be_sorted
    sorted_dict = OrderedDict()
    remaining = OrderedDict()
    for key in d: # first, sort items with no dependencies:
        if not d[key] or d[key]=='':
            sorted_dict[key] = d[key]
        else:
            remaining[key] = d[key]
    return (sorted_dict,remaining)

def sort_remaining(dict_to_be_sorted):
    sorted_dict = dict_to_be_sorted
    for key in sorted_dict: k_R_last = key
    for k_L in sorted_dict:
        for k_R in reversed(sorted_dict):
            L1 = any([k_R_last in x for x in sorted_dict[k_R]])
            L2 = any([k_L in x for x in sorted_dict[k_R_last]])
            if L1:
            # if L1 or L2:
                sorted_dict = swap_dict_by_keys(sorted_dict,k_R,k_R_last)
            k_R_last = k_R
    return sorted_dict

def swap_dict_by_keys(d,key1,key2):
    # d is defined in the following order
    # d[key1]
    # d[key2]
    # and sorted into the following order
    # d[key2]
    # d[key1]
    d_swapped = OrderedDict()
    for k in d:
        if k==key1:
            d_swapped[key2] = d[key2]
        elif k==key2:
            d_swapped[key1] = d[key1]
        else:
            d_swapped[k] = d[k]
    d = d_swapped
    return d

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
