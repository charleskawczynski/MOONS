import os
import funcs as func
import fortran_property as FP
from collections import OrderedDict

def longest_sub_string_match(string1, string2):
    if string1==string2:
      answer = string1
    else:
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

def longest_sub_string_match_list(L):
    s = L[0]
    for x in L[1:]: s = longest_sub_string_match(s,x)
    return s

def split_module(m,base_spaces):
    # Decomposes a module into parts
    BS = base_spaces
    m = [x.replace(BS,'') for x in m]
    preamble = [x for x in m if x.startswith('!')]
    module_name = [x for x in m if x.startswith('module ')]
    used_modules = [x for x in m if x.startswith('use ')]
    public_interfaces = [x for x in m if x.startswith('interface ')]
    public_data = [x for x in m if x.startswith('public :: ')]
    sub_routines = []
    passed_contains = False
    for s in m:
      if 'contains' in s: passed_contains=True
      if passed_contains:
        sub_routines.append(s)
    sub_routines = [x for x in sub_routines if not x.startswith('end module')]
    sub_routines = [x for x in sub_routines if not x.startswith('contains')]
    sub_routines = sub_routines[0:-1]

    type_defs = []
    passed_type_start = False
    passed_type_stop = False
    for s in m:
      if 'type ' in s: passed_type_start=True
      if passed_type_start and not passed_type_stop: type_defs.append(s)
      if 'end type' in s:
        passed_type_stop=True
        type_defs.append('')
    end_module = [x for x in m if x.startswith('end module')]
    module_name = [BS+x for x in module_name]
    used_modules = [BS+x for x in used_modules]
    public_data = [BS+x for x in public_data]
    public_interfaces = [BS+x for x in public_interfaces]
    type_defs = [BS+x for x in type_defs]
    sub_routines = [BS+x for x in sub_routines]
    end_module = [BS+x for x in end_module]
    return (preamble,module_name,used_modules,public_data,public_interfaces,type_defs,sub_routines,end_module)

def combine_modules(modules_dict,names,base_spaces):
    L = []
    split_mods = OrderedDict()
    modules_list = []
    for key in modules_dict:
      modules_list=modules_list+[modules_dict[key]]
    for n,m in zip(names,modules_list):
      split_mods[n] = split_module(m,base_spaces)
    PM=[]; MN=[]; UM=[]; PD=[]; PI=[]; TD=[]; SR=[]; EM=[]
    for key in split_mods:
      PM=PM+split_mods[key][0]
      MN=MN+split_mods[key][1]
      UM=UM+split_mods[key][2]
      PD=PD+split_mods[key][3]
      PI=PI+split_mods[key][4]
      TD=TD+split_mods[key][5]
      SR=SR+split_mods[key][6]
      EM=EM+split_mods[key][7]
    MN = longest_sub_string_match_list(MN)
    if MN.endswith('_'): MN = MN[:-1]
    if MN.startswith('_'): MN = MN[1:]
    MN = [MN+'_mod']
    UM = list(set(UM))
    PD = list(set(PD))
    EM = list(set(EM))
    L = L + MN
    L = L + UM + ['']
    L = L + PD + ['']
    L = L + align_interfaces(PI,base_spaces) + ['']
    L = L + TD
    L = L + [base_spaces+'contains']
    L = L + SR + ['']
    L = L + EM
    L = [x.replace(' ','') if x.replace(' ','')=='' else x for x in L]
    return L

def align_interfaces(L,base_spaces):
    spaces = ['' for x in range(50)]
    for i in range(len(spaces)):
        spaces[i] = ' '*i
    alias = [x.split('module procedure')[0].replace('interface','').replace(' ','').replace(';','') for x in L]
    sub_name = [x.split('module procedure')[1].replace('end interface','').replace(' ','').replace(';','') for x in L]
    st_al = [len(x) for x in alias]
    st_sn = [len(x) for x in sub_name]
    sp_al = [spaces[max(st_al)-x] for x in st_al]
    sp_sn = [spaces[max(st_sn)-x] for x in st_sn]
    temp = ['interface '+x+';'+s for x,s in zip(alias,sp_al)]
    temp = [x+'module procedure '+sn+';' for x,sn in zip(temp,sub_name)]
    temp = [x+s+'end interface' for x,s in zip(temp,sp_sn)]
    temp = [base_spaces+x for x in temp]
    L = temp
    return L
