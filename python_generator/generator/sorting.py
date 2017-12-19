from functools import reduce
import GOOFPY_directory as GD
import hashlib
import file_IO as IO
from collections import deque
from os import listdir
from os.path import isfile, join
import os
import copy
import sys
import shutil
import glob
import hashing as HASH
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
    L = topological_sort(d)
    return L

def sort_not_in_place(L):
    L.sort()
    return L

def topological_sort(dict_to_be_sorted):
    # sort separately
    L =[]
    (no_dependencies,remaining) = separate_non_dependencies(dict_to_be_sorted)
    remaining_sorted = sort_remaining(remaining)
    # multiple files that have the same "level" of dependency
    # are returned in a string list
    temp = [k for k in remaining_sorted]
    temp = [sort_not_in_place(x.split(' ')) for x in temp] # sort same dependency level
    temp = [item for sublist in temp for item in sublist]
    L = [k for k in no_dependencies]
    L.sort()
    L = L + temp
    return L

def separate_non_dependencies(dict_to_be_sorted):
    d = dict_to_be_sorted
    sorted_dict = OrderedDict()
    remaining = OrderedDict()
    for key in d: # first, sort items with no dependencies:
        if (not d[key]) or d[key]=='':
            sorted_dict[key] = d[key]
        else:
            remaining[key] = d[key]
    return (sorted_dict,remaining)

def sort_remaining(dict_to_be_sorted):
    return list(toposort2(dict_to_be_sorted))

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

# ----------------------------- OUT OF BOX, ONLINE -----------------------------
# ----------------------------- OUT OF BOX, ONLINE -----------------------------
# ----------------------------- OUT OF BOX, ONLINE -----------------------------

def toposort2(d):
    d_new = OrderedDict()
    for key in d:
        d_new[key] = set(' '.join(d[key]).split())
    data = d_new
    extra_items_in_deps = reduce(set.union, data.values()) - set(data.keys())
    data.update({item:set() for item in extra_items_in_deps})
    while True:
        ordered = set(item for item,dep in data.items() if not dep)
        if not ordered:
            break
        yield ' '.join(sorted(ordered))
        data = {item: (dep - ordered) for item,dep in data.items()
                if item not in ordered}
    assert not data, "A cyclic dependency exists amongst %r" % data
    return data
