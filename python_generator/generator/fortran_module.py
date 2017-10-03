import code_formatting as CF
import file_IO as IO
import os
import funcs as func
import fortran_property as FP
from collections import OrderedDict

suppress_all_warnings = True
# skip_structured_IO = ['TF']
make_dir_quiet = False
IO_structured_quiet = False
skip_structured_IO = ['SF','VF','TF']

use_necessary_for_restart = True
parent_necessary_for_restart = ['block_field']
child_necessary_for_restart = ['grid_field']

use_procedures = True
parent_to_reset_procedures = ['SF']
child_to_reset_procedures = ['block_field']

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


class fortran_module:

    def __init__(self):
        self.prop = OrderedDict()
        self.maxLineLength = 71
        self.implicitNone = 'implicit none'
        self.base_spaces = ' '*7
        # self.base_spaces = ''
        self.raw_lines = []
        self.raw_lines_used = False
        self.abstract_interfaces = []
        self.base_modules = []
        self.any_allocatables = False
        self.any_non_primitive_allocatables = False
        self.any_non_primitive_high_dimension = False
        self.only_primitives = False
        self.only_primitives_or_strings = False
        self.primitives_strings_or_procedures = False
        self.no_primitives = False
        self.any_primitive_allocatables = False
        self.spaces = ['' for x in range(50)]
        for i in range(len(self.spaces)):
            self.spaces[i] = ' '*i
        self.stars = ['' for x in range(30)]
        for i in range(len(self.stars)):
            self.stars[i] = '*'*i

    def set_base_spaces(self,base_spaces): self.base_spaces = base_spaces

    def set_name(self,name): self.name = name
    def set_folder_name(self,folder_name): self.folder_name = folder_name
    def set_default_real(self,default_real): self.default_real = default_real
    def get_name(self): return self.name

    def get_props(self): return self.prop

    def add_prop(self,name,class_,privacy,allocatable = False,rank = 1,dimension = 1,procedure = False):
        if type(name) is str:
            self.add_prop_string(name,class_,privacy,allocatable,rank,dimension,procedure)
        elif type(name) is list:
            for x in name: self.add_prop_string(x,class_,privacy,allocatable,rank,dimension,procedure)

    def add_prop_string(self,name,class_,privacy,allocatable,rank,dimension,procedure):
        prop = FP.fortran_property()
        prop.set_default_real(self.default_real)
        prop.init_remaining(name,class_,privacy,allocatable,rank,dimension,procedure)
        self.prop[prop.name] = prop
        non_primitive_allocatables = [self.prop[k].allocatable and not self.prop[k].object_type=='primitive' for k in self.prop]
        primitive_allocatables = [self.prop[k].allocatable and self.prop[k].object_type=='primitive' for k in self.prop]
        any_non_primitive_high_dimension = [self.prop[k].dimension>1 and not self.prop[k].object_type=='primitive' for k in self.prop]
        no_primitives = [not self.prop[k].object_type=='primitive' for k in self.prop]
        only_primitives = [self.prop[k].object_type=='primitive' for k in self.prop]
        only_primitives_or_strings = [self.prop[k].object_type=='primitive' or (self.prop[k].object_type=='object' and self.prop[k].class_=='string') for k in self.prop]
        primitives_strings_or_procedures = [self.prop[k].object_type=='primitive' or (self.prop[k].object_type=='object' and self.prop[k].class_=='string') or self.prop[k].object_type=='procedure' for k in self.prop]
        X = self.prop
        temp = [X[k] for k in X if X[k].object_type=='primitive' or (X[k].object_type=='object' and X[k].class_=='string')]
        self.any_non_primitive_allocatables = any(non_primitive_allocatables)
        self.any_primitive_allocatables = any(primitive_allocatables)
        self.any_non_primitive_high_dimension = any(any_non_primitive_high_dimension)
        self.no_primitives = all(no_primitives)
        self.only_primitives = all(only_primitives)
        self.only_primitives_or_strings = all(only_primitives_or_strings)
        self.primitives_strings_or_procedures = all(primitives_strings_or_procedures)

        self.any_allocatables = any([non_primitive_allocatables,primitive_allocatables])

    def print_props(self):
        for key in self.prop:
            self.prop[key].print()

    def set_list_of_classes(self,list_of_classes): self.list_of_classes = list_of_classes
    def get_list_of_classes(self): return self.list_of_classes

    def set_used_modules(self,used_modules): self.used_modules = used_modules
    def get_used_modules(self): return self.used_modules

    def read_raw_lines(self,file_name):
        self.raw_lines = IO.read_file_to_list(file_name,'read_raw_lines')
        temp = self.raw_lines
        temp = [x for x in temp if x.startswith('public ::')]
        temp = [x.replace('public ::','') for x in temp]
        temp = [x.replace(' ','') for x in temp]
        temp = [x.split(',') for x in temp]
        temp = [item for sublist in temp for item in sublist]
        self.abstract_interfaces = temp
        self.raw_lines_used = True

    def add_raw_lines(self,raw_lines):
        self.raw_lines = self.raw_lines+raw_lines
        self.raw_lines_used = True

    def get_suffix(self): return self.name
    # def get_suffix(self): return self.int_name
    # def get_suffix(self): return 'class'

    ################################################################################*/

    def contruct_fortran_module(self,class_list,abstract_interfaces,base_modules):
        self.class_list = class_list
        self.abstract_interfaces = abstract_interfaces
        self.base_modules = base_modules
        self.pre_process()
        L = self.get_fortran_module()
        return self.post_process(L)

    def pre_process(self):
        self.int_name = self.name
        # self.int_name = self.name[0:2]
        self.has_dir = []
        self.has_name = []
        X = self.prop
        self.has_name = any([X[key].name.lower()=='name' and X[key].class_.lower()=='string' for key in X])
        self.has_dir  = any([X[key].name.lower()=='dir'  and X[key].class_.lower()=='string' for key in X])
        self.has_dir_name = self.has_dir and self.has_name
        for key in self.prop:
          self.prop[key].set_do_loop_iter()
          self.prop[key].set_spaces(self.spaces)
        # for key in self.prop:
        #   L = ['dir' in self.prop[key].name.lower()]
        #   L = L + ['string' in self.prop[key].class_.lower()]
        #   self.has_dir = self.has_dir + [all(L)]
        # for key in self.prop:
        #   L = ['name' in self.prop[key].name.lower()]
        #   L = L + ['string' in self.prop[key].class_.lower()]
        #   self.has_name = self.has_name + [all(L)]
        # self.has_dir_name = any(self.has_dir) and any(self.has_name)

    def get_fortran_module(self):
        c = []
        c=c+['! ***************************************************']
        c=c+['! ******* THIS CODE IS GENERATED. DO NOT EDIT *******']
        c=c+['! ***************************************************']
        c=c+['module ' + self.get_name() + '_mod']
        c=c+[self.write_used_modules()]
        c=c+[[self.implicitNone]+['']]
        c=c+[['private']]
        c=c+[['public :: '+self.name]]
        c=c+[['public :: init,delete,display,print,export,import']]
        c=c+[['public :: display_short,print_short']+['']]
        c=c+[['public :: export_primitives,import_primitives']+['']]
        c=c+[['public :: export_structured,import_structured']+['']]
        c=c+[['public :: set_IO_dir,make_IO_dir']+['']]
        if suppress_all_warnings:
            c=c+[['public :: suppress_warnings']+['']]
        c=c+[self.write_interfaces()+['']]
        c=c+[self.class_definition()]
        c=c+[['end type']+['']]
        c=c+[['contains']]
        c=c+[self.write_all_functions()]
        c=c+['end module']
        return c

    def post_process(self,L):
        L = func.flatten(L)
        L = [x for x in L if not x==None]
        if self.raw_lines_used:
          L = self.raw_lines
        else:
          L = [self.base_spaces+x if not (x=='' or x.startswith('#')) else x for x in L]
        # L = [self.base_spaces+x for x in L]
        # L = [CF.indent_lines(k) for k in L]
        L = CF.indent_lines(L)
        # L = [self.breakLine(k,[]) for k in L]
        return L

    def write_used_modules(self):
        d=[]
        d=d
        types = [self.prop[k].get_class() for k in self.prop]
        self.any_cp = any(['cp' in x for x in types])
        c = [self.used_modules]
        c = [item for sublist in c for item in sublist]
        if self.any_cp:
            c=['current_precision_mod']+c
        for key in self.prop:
            d=d+[[x for x in self.class_list if self.prop[key].get_class().lower()==x.lower()]]
            d=d+[[x for x in self.base_modules]]
            d=d+[[x for x in self.base_modules if self.prop[key].get_class().lower()==x.lower()]]
            for k in self.abstract_interfaces:
                d=d+[[k for x in self.abstract_interfaces[k] if self.prop[key].get_class().lower()==x.lower()]]

        if use_procedures:
          if any([x==self.name for x in parent_to_reset_procedures]):
            for key in self.prop:
              if any([x==self.prop[key].class_ for x in child_to_reset_procedures]):
                d=d+[[self.prop[key].class_+'_extend']]

        d = list(set([item for sublist in d for item in sublist if item]))
        d.sort()
        c = c+[x+'_mod' for x in d]
        return ['use '+x if x else None for x in c]

    def write_interfaces(self):
        alias = []
        sub_name = []
        alias = alias+['init'];              sub_name = sub_name+['init_copy'];
        alias = alias+['delete'];            sub_name = sub_name+['delete'];
        alias = alias+['display'];           sub_name = sub_name+['display'];
        alias = alias+['display_short'];     sub_name = sub_name+['display_short'];
        alias = alias+['display'];           sub_name = sub_name+['display_wrap'];
        alias = alias+['print'];             sub_name = sub_name+['print'];
        alias = alias+['print_short'];       sub_name = sub_name+['print_short'];
        alias = alias+['export'];            sub_name = sub_name+['export'];
        alias = alias+['export_primitives']; sub_name = sub_name+['export_primitives'];
        alias = alias+['import'];            sub_name = sub_name+['import'];
        alias = alias+['export_structured']; sub_name = sub_name+['export_structured_D'];
        alias = alias+['import_structured']; sub_name = sub_name+['import_structured_D'];
        alias = alias+['import_primitives']; sub_name = sub_name+['import_primitives'];
        alias = alias+['export'];            sub_name = sub_name+['export_wrap'];
        alias = alias+['import'];            sub_name = sub_name+['import_wrap'];
        alias = alias+['set_IO_dir'];        sub_name = sub_name+['set_IO_dir'];
        alias = alias+['make_IO_dir'];       sub_name = sub_name+['make_IO_dir'];

        if suppress_all_warnings:
            alias = alias+['suppress_warnings']; sub_name = sub_name+['suppress_warnings'];

        if self.has_dir_name:
          alias = alias+['export'];          sub_name = sub_name+['export_DN'];
          alias = alias+['import'];          sub_name = sub_name+['import_DN'];
          alias = alias+['export_structured']; sub_name = sub_name+['export_structured_DN'];
          alias = alias+['import_structured']; sub_name = sub_name+['import_structured_DN'];
        st_al = [len(x) for x in alias]
        st_sn = [len(x) for x in sub_name]
        sp_al = [self.spaces[max(st_al)-x] for x in st_al]
        sp_sn = [self.spaces[max(st_sn)-x] for x in st_sn]
        c = ['interface '+x+';'+s for x,s in zip(alias,sp_al)]
        # c = [x+'module procedure '+sn+'_'+self.name+';' for x,sn in zip(c,sub_name)]
        c = [x+'module procedure '+sn+'_'+self.get_suffix()+';' for x,sn in zip(c,sub_name)]
        c = [x+s+'end interface' for x,s in zip(c,sp_sn)]
        return c

    ################################################################################*/

    def write_all_functions(self):
        c = []
        c=c+['']
        self.set_arg_objects()
        self.set_arg_list()
        c=c+[self.init_copy()+['']]
        c=c+[self.init_delete()+['']]

        c=c+[self.display_module()+['']]
        c=c+[self.display_short_module()+['']]
        c=c+[self.display_wrap_module()+['']]

        c=c+[self.print_module()+['']]
        c=c+[self.print_short_module()+['']]

        c=c+[self.export_module_un(False)+['']] # export(this,un)
        c=c+[self.import_module_un(False)+['']] # import(this,un)
        c=c+[self.export_module_un(True)+['']]  # export_primitives(this,un)
        c=c+[self.import_module_un(True)+['']]  # import_primitives(this,un)
        c=c+[self.export_wrap_module()+['']]    # export(this,dir,name)
        c=c+[self.import_wrap_module()+['']]    # import(this,dir,name)

        if self.has_dir_name:
          c=c+[self.export_DN_module()+['']]    # export(this), if has (dir,name)
          c=c+[self.import_DN_module()+['']]    # import(this), if has (dir,name)
          c=c+[self.export_structured_DN_module()+['']] # export_structured(this,dir), names are determined by data structure
          c=c+[self.import_structured_DN_module()+['']] # import_structured(this,dir), names are determined by data structure

        c=c+[self.set_IO_dir_module()+['']]
        c=c+[self.make_IO_dir_module()+['']]
        c=c+[self.export_structured_D_module()+['']] # export_structured(this), if has (dir,name)
        c=c+[self.import_structured_D_module()+['']] # import_structured(this), if has (dir,name)


        if suppress_all_warnings:
            c=c+[self.suppress_fortran_warnings_module()+['']]
        return c

    def class_definition(self):
        c = ['type ' + self.name]
        p = [self.prop[k].get_privacy() for k in self.prop]
        self.all_private = all([x=='private' for x in p])
        self.all_public = all([x=='public' for x in p])
        if self.all_private:
          c=c+[self.spaces[2]+'private']
        # if self.all_public:
        #     c=c+[self.spaces[2]+'public']
        for key in self.prop:
          c=c+[[self.spaces[2]+x for x in self.prop[key].write_class_definition(self.all_private,self.all_public)]]
        return c

    def init_copy(self):
        sig = 'init_copy_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,that')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this']
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: that']
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c=c+L
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c=c+L
        c=c+[self.spaces[2] + 'call delete(this)']
        for key in self.arg_objects:
            c=c+[self.spaces[2]+x for x in self.arg_objects[key].write_init_copy()]
        c=c+[self.end_sub()]
        return c

    def init_delete(self):
        sig = 'delete_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this')]
        c=c+[self.spaces[2] + self.implicitNone ]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c=c+[L]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c=c+[L]
        for key in self.arg_objects:
            c=c+[[self.spaces[2]+x for x in self.arg_objects[key].write_delete()]]
        c=c+[self.end_sub()]
        return c

    def display_module(self):
        sig = 'display_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,un')]
        self.set_arg_objects()
        self.set_arg_list()
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        c=c+[self.spaces[2] + 'integer,intent(in) :: un' ]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c=c+[L]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c=c+[L]
        st_n = [len(self.prop[key].name) for key in self.prop]
        sp_n = [self.spaces[max(st_n)-x] for x in st_n]
        for key,s in zip(self.prop,sp_n):
            self.prop[key].set_display_spaces(s)
        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_display()]]
        c=c+[self.end_sub()]
        return c

    def display_short_module(self):
        sig = 'display_short_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,un')]
        self.set_arg_objects()
        self.set_arg_list()
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        c=c+[self.spaces[2] + 'integer,intent(in) :: un' ]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c=c+[L]
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c=c+[L]
        st_n = [len(self.prop[key].name) for key in self.prop]
        sp_n = [self.spaces[max(st_n)-x] for x in st_n]
        for key,s in zip(self.prop,sp_n):
            self.prop[key].set_display_spaces(s)

        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_display_short()]]
        c=c+[self.end_sub()]
        return c

    def print_module(self):
        sig = 'print_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        c=c+[self.spaces[2] + "call display(this,6)"]
        c=c+[self.end_sub() ]
        return c

    def print_short_module(self):
        sig = 'print_short_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        c=c+[self.spaces[2] + "call display_short(this,6)"]
        c=c+[self.end_sub() ]
        return c

    def export_module_un(self,primitives_only):
        if primitives_only:
            sig = 'export_primitives_' + self.get_suffix()
        else:
            sig = 'export_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,un')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        c=c+[self.spaces[2] + 'integer,intent(in) :: un' ]
        if not primitives_only:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_iterators()
                if L: c=c+[L]
        if any([not primitives_only,primitives_only and self.any_primitive_allocatables]):
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c=c+[L]

        if suppress_all_warnings:
            if primitives_only and self.no_primitives:
                c=c+[self.spaces[2] + 'integer :: un_suppress_warning' ]
                c=c+[self.spaces[2] + 'un_suppress_warning = un' ]
                c=c+[self.spaces[2] + 'call suppress_warnings(this)' ]

        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_export(primitives_only)]]
        c=c+[self.end_sub()]
        return c

    def import_module_un(self,primitives_only):
        if primitives_only:
            sig = 'import_primitives_' + self.get_suffix()
        else:
            sig = 'import_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,un')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        c=c+[self.spaces[2] + 'integer,intent(in) :: un' ]
        if not primitives_only:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_iterators()
                if L: c=c+[L]
        if any([not primitives_only,primitives_only and self.any_primitive_allocatables]):
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c=c+[L]
        if suppress_all_warnings:
            if primitives_only and self.no_primitives:
                c=c+[self.spaces[2] + 'integer :: un_suppress_warning' ]
                c=c+[self.spaces[2] + 'un_suppress_warning = un' ]
                c=c+[self.spaces[2] + 'call suppress_warnings(this)' ]

        if not primitives_only:
            c=c+[self.spaces[2] + 'call delete(this)' ]
        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_import(primitives_only)]]
        c=c+[self.end_sub()]
        return c

    def display_wrap_module(self):
        sig = 'display_wrap_' + self.get_suffix()
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = new_and_open(dir,name)']
        L=L+[self.spaces[2]+'call display(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def export_wrap_module(self):
        name_list = []
        for key in self.prop:
          name_list = name_list+[self.prop[key].name]
        special_arg_list = ['x','y','z']
        if all([x in special_arg_list for x in name_list]):
          L = self.export_wrap_module_specialized()
        else:
          L = self.export_wrap_module_standard()
        return L

    def export_wrap_module_standard(self):
        sig = 'export_wrap_' + self.get_suffix()
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = new_and_open(dir,name)']
        L=L+[self.spaces[2]+'call export(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def export_wrap_module_specialized(self):
        sig = 'export_wrap_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,dir,name')]
        c=c+[self.spaces[2]+self.implicitNone]
        c=c+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        c=c+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_export_wrap_specialized()]]
        c=c+[self.end_sub()]
        return c

    def import_wrap_module(self):
        name_list = []
        for key in self.prop:
          name_list = name_list+[self.prop[key].name]
        special_arg_list = ['x','y','z']
        if all([x in special_arg_list for x in name_list]):
          L = self.import_wrap_module_specialized()
        else:
          L = self.import_wrap_module_standard()
        return L

    def import_wrap_module_standard(self):
        sig = 'import_wrap_' + self.get_suffix()
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(inout) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = open_to_read(dir,name)']
        L=L+[self.spaces[2]+'call export(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def import_wrap_module_specialized(self):
        sig = 'import_wrap_' + self.get_suffix()
        c = [self.full_sub_signature(sig,'this,dir,name')]
        c=c+[self.spaces[2]+self.implicitNone]
        c=c+[self.spaces[2]+'type('+self.name+'),intent(inout) :: this']
        c=c+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        for key in self.prop:
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_import_wrap_specialized()]]
        c=c+[self.end_sub()]
        return c

    def export_DN_module(self):
        L = []
        sig = 'export_DN_' + self.get_suffix()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        L=L+[self.spaces[2]+'call export(this,str(this%dir),str(this%name))']
        L=L+[self.end_sub()]
        return L

    def import_DN_module(self):
        L = []
        sig = 'import_DN_' + self.get_suffix()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(inout) :: this']
        L=L+[self.spaces[2]+'type(string) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'call init(dir,this%dir)']
        L=L+[self.spaces[2]+'call init(name,this%name)']
        L=L+[self.spaces[2]+'un = open_to_read(str(dir),str(name))']
        L=L+[self.spaces[2]+'call import(this,un)']
        L=L+[self.spaces[2]+'call delete(dir)']
        L=L+[self.spaces[2]+'call delete(name)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def export_structured_D_module(self):
        sig = 'export_structured_D_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = []
        c=c+[self.full_sub_signature(sig,'this,dir')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this']
        c=c+[self.spaces[2] + 'character(len=*),intent(in) :: dir']
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+L
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+L
        c=c+[self.spaces[2] + "integer :: un"]
        if not IO_structured_quiet:
          c=c+[self.spaces[2] + "write(*,*) 'Exporting "+ self.name +" structured'"]
        c=c+[self.spaces[2] + "un = new_and_open(dir,'primitives')"]
        c=c+[self.spaces[2] + "call export_primitives(this,un)"]
        c=c+[self.spaces[2]+'close(un)']
        for key in self.prop:
          if all([not self.prop[key].class_==x for x in skip_structured_IO]):
            temp1 = any([self.name==x for x in parent_necessary_for_restart])
            temp2 = any([self.prop[key].class_==x for x in child_necessary_for_restart])
            if temp1 and temp2 and use_necessary_for_restart:
              c=c+[self.spaces[2] + "if (this%necessary_for_restart) then" ]
            c=c+[self.spaces[2]+x for x in self.prop[key].write_export_structured('dir')]
            if temp1 and temp2 and use_necessary_for_restart:
              c=c+[self.spaces[2] + "endif" ]
        c=c+[self.end_sub()]
        return c

    def export_structured_DN_module(self):
        sig = 'export_structured_DN_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = [self.full_sub_signature(sig,'this')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+[L]
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+[L]
        c=c+[self.spaces[2] + "integer :: un" ]
        c=c+[self.spaces[2] + "un = new_and_open(str(this%dir),'primitives')" ]
        c=c+[self.spaces[2] + "call export_primitives(this,un)" ]
        c=c+[self.spaces[2]+'close(un)']
        for key in self.prop:
          if all([not self.prop[key].class_==x for x in skip_structured_IO]):
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_export_structured('str(this%dir)')]]
        c=c+[self.end_sub()]
        return c

    def import_structured_D_module(self):
        sig = 'import_structured_D_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        c=c+[self.spaces[2] + 'character(len=*),intent(in) :: dir' ]
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+[L]
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+[L]
        c=c+[self.spaces[2] + "integer :: un" ]
        if not IO_structured_quiet:
          c=c+[self.spaces[2] + "write(*,*) 'Importing "+ self.name +" structured'"]
        c=c+[self.spaces[2] + "un = open_to_read(dir,'primitives')" ]
        c=c+[self.spaces[2] + "call import_primitives(this,un)" ]
        c=c+[self.spaces[2]+'close(un)']
        for key in self.prop:
          if all([not self.prop[key].class_==x for x in skip_structured_IO]):
            temp1 = any([self.name==x for x in parent_necessary_for_restart])
            temp2 = any([self.prop[key].class_==x for x in child_necessary_for_restart])
            if temp1 and temp2 and use_necessary_for_restart:
              c=c+[self.spaces[2] + "if (this%necessary_for_restart) then" ]
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_import_structured('dir')]]
            if temp1 and temp2 and use_necessary_for_restart:
              c=c+[self.spaces[2] + "endif" ]

        # Custom, implementation detail for procedures:
        if use_procedures:
          if any([x==self.name for x in parent_to_reset_procedures]):
            for key in self.prop:
              if any([x==self.prop[key].class_ for x in child_to_reset_procedures]):
                c=c+[[self.spaces[2]+x for x in self.prop[key].write_set_procedures()]]

        c=c+[self.end_sub()]
        return c

    def import_structured_DN_module(self):
        sig = 'import_structured_DN_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = [self.full_sub_signature(sig,'this')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+[L]
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+[L]
        c=c+[self.spaces[2] + "integer :: un" ]
        c=c+[self.spaces[2] + "un = open_to_read(str(this%dir),'primitives')" ]
        c=c+[self.spaces[2] + "call import_primitives(this,un)" ]
        c=c+[self.spaces[2]+'close(un)']
        for key in self.prop:
          if all([not self.prop[key].class_==x for x in skip_structured_IO]):
            c=c+[[self.spaces[2]+x for x in self.prop[key].write_import_structured('str(this%dir)')]]
        c=c+[self.end_sub()]
        return c

    def set_IO_dir_module(self):
        sig = 'set_IO_dir_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        c=c+[self.spaces[2] + 'character(len=*),intent(in) :: dir' ]
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+[L]
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+[L]
        if suppress_all_warnings:
          c=c+[self.spaces[2] + 'call suppress_warnings(this)' ]
        if suppress_all_warnings and self.primitives_strings_or_procedures:
          c=c+[self.spaces[2] + 'if (.false.) then' ]
          c=c+[self.spaces[2] + 'write(*,*) dir' ]
          c=c+[self.spaces[2] + 'endif' ]
        if self.has_dir_name:
          c=c+[self.spaces[2] + "call init(this%dir,dir)" ]
          c=c+[self.spaces[2] + "call init(this%name,'primitives')" ]
        for key in self.prop:
          c=c+[[self.spaces[2]+x for x in self.prop[key].write_set_IO_dir('set_IO_dir')]]
        c=c+[self.end_sub()]
        return c

    def make_IO_dir_module(self):
        sig = 'make_IO_dir_' + self.get_suffix()
        # sig = 'set_IO_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c=c+[self.spaces[2] + self.implicitNone]
        c=c+[self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' ]
        c=c+[self.spaces[2] + 'character(len=*),intent(in) :: dir' ]
        for key in self.arg_objects:
          L = self.arg_objects[key].get_list_of_local_iterators()
          if L: c=c+[L]
        if self.any_non_primitive_high_dimension:
          for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L: c=c+[L]
        if suppress_all_warnings:
          c=c+[self.spaces[2] + 'call suppress_warnings(this)' ]
        if make_dir_quiet:
          c=c+[self.spaces[2] + 'call make_dir_quiet(dir)' ]
        else:
          c=c+[self.spaces[2] + 'call make_dir(dir)' ]
        if self.has_dir_name:
          c=c+[self.spaces[2] + "call init(this%dir,dir)" ]
          c=c+[self.spaces[2] + "call init(this%name,'primitives')" ]
        for key in self.prop:
          c=c+[[self.spaces[2]+x for x in self.prop[key].write_set_IO_dir('make_IO_dir')]]
        c=c+[self.end_sub()]
        return c

    def suppress_fortran_warnings_module(self):
        if suppress_all_warnings:
          sig = 'suppress_warnings_' + self.get_suffix()
          c = [self.full_sub_signature(sig,'this')]
          c=c+[self.spaces[2] + self.implicitNone]
          c=c+[self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' ]
          c=c+[self.spaces[2] + 'if (.false.) then' ]
          c=c+[self.spaces[2] + 'call print(this)' ]
          c=c+[self.spaces[2] + 'endif' ]
          c=c+[self.end_sub()]
        return c

    ################################################################################*/

    def full_func_signature(self,sig,args,result): return 'function ' + sig + '(' + args + ') result(' + result + ')'
    def full_sub_signature(self,sig,args): return 'subroutine ' + sig + '(' + args + ')'
    def end_function(self,function = False): return 'end function'
    def end_sub(self,function = False): return 'end subroutine'
    def set_arg_objects(self): self.arg_objects = self.prop
    def set_arg_list(self): self.arg_list = [key for key in self.prop]

    ################################################################################*/

    def breakLine(self,stringList,result = []):
        spaces = self.base_spaces[:-2]
        if (len(stringList) >= self.maxLineLength):
            strMax = stringList[0:self.maxLineLength]
            # If commas exist in the function signature, then break the line down
            # If this is still a problem then reduce the size of the function name or property names
            if ')' in strMax:
                cutoff = strMax.rfind(')')+1
            elif ',' in strMax:
                cutoff = strMax.rfind(',')+1
            elif '(' in strMax:
                cutoff = strMax.rfind('(')+1
            else:
                result = stringList

            if any(x in strMax for x in [',','(',')']):
                strCut = strMax[0:cutoff]
                strRemain = stringList[cutoff:]
                if not (len(strRemain.replace(' ','')) <= len('')):
                    result.append(strCut)
                    strRemain = spaces + '&   ' + strRemain
                    self.breakLine(strRemain,result)
        else:
            result.append(stringList)
        return result

    ################################################################################*/
