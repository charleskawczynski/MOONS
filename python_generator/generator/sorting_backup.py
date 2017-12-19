import GOOFPY_directory as GD
import file_IO as IO
from os import listdir
from os.path import isfile, join
import os
import copy
import sys
import shutil
import glob
from collections import OrderedDict
import re

def get_dependency_dict(file_list,PS_file_sys):
    d = OrderedDict()
    SL = []
    for f in file_list:
        FN = f.split(PS_file_sys)[-1].replace('.f90','')
        MNs = IO.read_file_to_list(f,'sort_files_by_dependency')
        MNs = [x for x in MNs if not x.startswith('c')]
        MNs = [x.lstrip() for x in MNs]
        MNs = [x.rstrip() for x in MNs]
        MNs = [x for x in MNs if not x.startswith('!')]
        MNs = [x for x in MNs if x.startswith('use ')]
        MNs = [x.replace('use ','') for x in MNs]
        MNs = [x.replace('_mod','') for x in MNs]
        d[FN] = MNs
    return d

def sort_files_by_dependency(file_list,PS_file_sys):
    d = get_dependency_dict(file_list,PS_file_sys)
    return sort_direct_acyclic_graph(d)

def sort_files_by_dependency_recent(file_list,module_names):
    d = OrderedDict()
    SL = []
    for fn,mn in zip(file_list,module_names):
        L = IO.read_file_to_list(fn,'sort_files_by_dependency')
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
