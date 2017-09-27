import os
import funcs as func
import fortran_property as FP
from collections import OrderedDict

suppress_all_warnings = True

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

def indent_lines(L):
  indent = '  '
  T_up = ('if','do')
  T_dn = ('endif','enddo')
  indent_cumulative = ''
  s_indent = len(indent)
  temp = ['' for x in L]
  for i,x in enumerate(L):
    if x.startswith(T_dn):
      indent_cumulative = indent_cumulative[s_indent:]
    temp[i]=indent_cumulative+temp[i]
    if x.startswith(T_up):
      indent_cumulative = indent_cumulative + indent
  L = [s+x for (s,x) in zip(temp,L)]
  return L

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
        self.any_non_primitive_allocatables = any(non_primitive_allocatables)
        self.any_primitive_allocatables = any(primitive_allocatables)
        self.any_non_primitive_high_dimension = any(any_non_primitive_high_dimension)
        self.no_primitives = all(no_primitives)

        self.any_allocatables = any([non_primitive_allocatables,primitive_allocatables])

    def print_props(self):
        for key in self.prop:
            self.prop[key].print()

    def set_list_of_classes(self,list_of_classes): self.list_of_classes = list_of_classes
    def get_list_of_classes(self): return self.list_of_classes

    def set_used_modules(self,used_modules): self.used_modules = used_modules
    def get_used_modules(self): return self.used_modules

    def read_raw_lines(self,file_name):
        self.raw_lines = func.read_file_to_list(file_name,'read_raw_lines')
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
        for key in self.prop:
          self.prop[key].set_do_loop_iter()
          self.prop[key].set_spaces(self.spaces)
        for key in self.prop:
          L = ['dir' in self.prop[key].name.lower()]
          L = L + ['string' in self.prop[key].class_.lower()]
          self.has_dir = self.has_dir + [all(L)]
        for key in self.prop:
          L = ['name' in self.prop[key].name.lower()]
          L = L + ['string' in self.prop[key].class_.lower()]
          self.has_name = self.has_name + [all(L)]
        self.has_dir_name = any(self.has_dir) and any(self.has_name)

    def get_fortran_module(self):
        c = []
        c.append('! ***************************************************')
        c.append('! ******* THIS CODE IS GENERATED. DO NOT EDIT *******')
        c.append('! ***************************************************')
        c.append('module ' + self.get_name() + '_mod')
        c.append(self.write_used_modules())
        c.append([self.implicitNone]+[''])
        c.append(['private'])
        c.append(['public :: '+self.name])
        c.append(['public :: init,delete,display,print,export,import'])
        c.append(['public :: display_short,print_short']+[''])
        c.append(['public :: export_primitives,import_primitives']+[''])
        c.append(['public :: export_restart,import_restart']+[''])
        c.append(['public :: make_restart_dir']+[''])
        if suppress_all_warnings:
            c.append(['public :: suppress_warnings']+[''])
        c.append(self.write_interfaces()+[''])
        c.append(self.class_definition())
        c.append(['end type']+[''])
        c.append(['contains'])
        c.append(self.write_all_functions())
        c.append('end module')
        return c

    def post_process(self,c):
        l = func.flatten(c)
        l = [x for x in l if not x==None]
        if self.raw_lines_used:
          l = self.raw_lines
        else:
          l = [self.base_spaces+x if not (x=='' or x.startswith('#')) else x for x in l]
        # l = [self.base_spaces+x for x in l]
        s = [self.breakLine(k,[]) for k in l]
        s = l
        return s

    def write_used_modules(self):
        dependent=[]
        types = [self.prop[k].get_class() for k in self.prop]
        self.any_cp = any(['cp' in x for x in types])
        c = [self.used_modules]
        c = [item for sublist in c for item in sublist]
        if self.any_cp:
            c=['current_precision_mod']+c
        for key in self.prop:
            dependent.append([x for x in self.class_list if self.prop[key].get_class().lower()==x.lower()])
            dependent.append([x for x in self.base_modules])
            dependent.append([x for x in self.base_modules if self.prop[key].get_class().lower()==x.lower()])
            for k in self.abstract_interfaces:
                dependent.append([k for x in self.abstract_interfaces[k] if self.prop[key].get_class().lower()==x.lower()])
        dependent = list(set([item for sublist in dependent for item in sublist if item]))
        dependent.sort()
        c = c+[x+'_mod' for x in dependent]
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
        alias = alias+['export_restart'];    sub_name = sub_name+['export_restart'];
        alias = alias+['import'];            sub_name = sub_name+['import'];
        alias = alias+['import_restart'];    sub_name = sub_name+['import_restart'];
        alias = alias+['import_primitives']; sub_name = sub_name+['import_primitives'];
        alias = alias+['export'];            sub_name = sub_name+['export_wrap'];
        alias = alias+['import'];            sub_name = sub_name+['import_wrap'];
        alias = alias+['make_restart_dir'];  sub_name = sub_name+['make_restart_dir'];

        if suppress_all_warnings:
            alias = alias+['suppress_warnings']; sub_name = sub_name+['suppress_warnings'];

        if self.has_dir_name:
          alias = alias+['export'];          sub_name = sub_name+['export_DN'];
          alias = alias+['import'];          sub_name = sub_name+['import_DN'];
        st_al = [len(x) for x in alias]
        st_sn = [len(x) for x in sub_name]
        sp_al = [self.spaces[max(st_al)-x] for x in st_al]
        sp_sn = [self.spaces[max(st_sn)-x] for x in st_sn]
        c = ['interface '+x+';'+s for x,s in zip(alias,sp_al)]
        # c = [x+'module procedure '+sn+'_'+self.name+';' for x,sn in zip(c,sub_name)]
        c = [x+'module procedure '+sn+'_'+self.int_name+';' for x,sn in zip(c,sub_name)]
        c = [x+s+'end interface' for x,s in zip(c,sp_sn)]
        return c

    ################################################################################*/

    def write_all_functions(self):
        c = []
        c.append('')
        self.set_arg_objects()
        self.set_arg_list()
        c.append(self.init_copy()+[''])
        c.append(self.init_delete()+[''])

        c.append(self.display_module()+[''])
        c.append(self.display_short_module()+[''])
        c.append(self.display_wrap_module()+[''])

        c.append(self.print_module()+[''])
        c.append(self.print_short_module()+[''])

        c.append(self.export_module(True)+[''])
        c.append(self.export_module(False)+[''])
        c.append(self.import_module(True)+[''])
        c.append(self.import_module(False)+[''])
        c.append(self.export_wrap_module()+[''])
        c.append(self.import_wrap_module()+[''])
        if self.has_dir_name:
          c.append(self.export_DN()+[''])
          c.append(self.import_DN()+[''])

        c.append(self.make_restart_dir_module()+[''])
        c.append(self.export_restart_module()+[''])
        c.append(self.import_restart_module()+[''])

        if suppress_all_warnings:
            c.append(self.suppress_fortran_warnings_module()+[''])
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
            c.append([self.spaces[2]+x for x in self.prop[key].write_class_definition(self.all_private,self.all_public)])
        return c

    def init_copy(self):
        sig = 'init_copy_' + self.int_name
        c = [self.full_sub_signature(sig,'this,that')]
        c.append(self.spaces[2] + self.implicitNone )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: that' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c.append(L)
        c.append(self.spaces[2] + 'call delete(this)')
        for key in self.arg_objects:
            c.append([self.spaces[2]+x for x in self.arg_objects[key].write_init_copy()])
        c.append(self.end_sub())
        return c

    def init_delete(self):
        sig = 'delete_' + self.int_name
        c = [self.full_sub_signature(sig,'this')]
        c.append(self.spaces[2] + self.implicitNone )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c.append(L)
        for key in self.arg_objects:
            c.append([self.spaces[2]+x for x in self.arg_objects[key].write_delete()])
        c.append(self.end_sub())
        return c

    def display_module(self):
        sig = 'display_' + self.int_name
        c = [self.full_sub_signature(sig,'this,un')]
        self.set_arg_objects()
        self.set_arg_list()
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c.append(L)
        st_n = [len(self.prop[key].name) for key in self.prop]
        sp_n = [self.spaces[max(st_n)-x] for x in st_n]
        for key,s in zip(self.prop,sp_n):
            self.prop[key].set_display_spaces(s)

        # pass # temporary
        # c.append(self.spaces[2] + "write(un,*) ' -------------------- " +self.name+ "'" )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_display()])
        c.append(self.end_sub())
        return c

    def display_short_module(self):
        sig = 'display_short_' + self.int_name
        c = [self.full_sub_signature(sig,'this,un')]
        self.set_arg_objects()
        self.set_arg_list()
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_shape()
            if L and not self.arg_objects[key].object_type=='primitive': c.append(L)
        st_n = [len(self.prop[key].name) for key in self.prop]
        sp_n = [self.spaces[max(st_n)-x] for x in st_n]
        for key,s in zip(self.prop,sp_n):
            self.prop[key].set_display_spaces(s)

        # c.append(self.spaces[2] + "write(un,*) ' -------------------- " +self.name+ "'" )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_display_short()])
        c.append(self.end_sub())
        return c

    def print_module(self):
        sig = 'print_' + self.int_name
        c = [self.full_sub_signature(sig,'this')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + "call display(this,6)")
        c.append(self.end_sub() )
        return c

    def print_short_module(self):
        sig = 'print_short_' + self.int_name
        c = [self.full_sub_signature(sig,'this')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + "call display_short(this,6)")
        c.append(self.end_sub() )
        return c

    def export_module(self,primitives_only):
        if primitives_only:
            sig = 'export_primitives_' + self.int_name
        else:
            sig = 'export_' + self.int_name
        c = [self.full_sub_signature(sig,'this,un')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        if not primitives_only:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_iterators()
                if L: c.append(L)
        if any([not primitives_only,primitives_only and self.any_primitive_allocatables]):
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c.append(L)

        if suppress_all_warnings:
            if primitives_only and self.no_primitives:
                c.append(self.spaces[2] + 'integer :: un_suppress_warning' )
                c.append(self.spaces[2] + 'un_suppress_warning = un' )
                c.append(self.spaces[2] + 'call suppress_warnings(this)' )

        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_export(primitives_only)])
        c.append(self.end_sub())
        return c

    def import_module(self,primitives_only):
        if primitives_only:
            sig = 'import_primitives_' + self.int_name
        else:
            sig = 'import_' + self.int_name
        c = [self.full_sub_signature(sig,'this,un')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        if not primitives_only:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_iterators()
                if L: c.append(L)
        if any([not primitives_only,primitives_only and self.any_primitive_allocatables]):
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c.append(L)
        if suppress_all_warnings:
            if primitives_only and self.no_primitives:
                c.append(self.spaces[2] + 'integer :: un_suppress_warning' )
                c.append(self.spaces[2] + 'un_suppress_warning = un' )
                c.append(self.spaces[2] + 'call suppress_warnings(this)' )

        if not primitives_only:
            c.append(self.spaces[2] + 'call delete(this)' )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_import(primitives_only)])
        c.append(self.end_sub())
        return c

    def display_wrap_module(self):
        sig = 'display_wrap_' + self.int_name
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
        sig = 'export_wrap_' + self.int_name
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
        sig = 'export_wrap_' + self.int_name
        c = [self.full_sub_signature(sig,'this,dir,name')]
        c=c+[self.spaces[2]+self.implicitNone]
        c=c+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        c=c+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_export_wrap_specialized()])
        c.append(self.end_sub())
        return c

    def import_wrap_module(self):
        sig = 'import_wrap_' + self.int_name
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(inout) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = open_to_read(dir,name)']
        L=L+[self.spaces[2]+'call import(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def export_DN(self):
        L = []
        sig = 'export_DN_' + self.int_name
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        L=L+[self.spaces[2]+'call export(this,str(this%dir),str(this%name))']
        L=L+[self.end_sub()]
        return L

    def import_DN(self):
        L = []
        sig = 'import_DN_' + self.int_name
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

    def export_restart_module(self):
        sig = 'export_restart_' + self.get_name()
        # sig = 'make_restart_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'character(len=*),intent(in) :: dir' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        if self.any_non_primitive_high_dimension:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c.append(L)
        c.append(self.spaces[2] + "integer :: un" )
        c.append(self.spaces[2] + "un = new_and_open(dir,'primitives')" )
        c.append(self.spaces[2] + "call export_primitives(this,un)" )
        c.append(self.spaces[2] + "close(un)" )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_export_restart()])
        c.append(self.end_sub())
        return c

    def import_restart_module(self):
        sig = 'import_restart_' + self.get_name()
        # sig = 'make_restart_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        c.append(self.spaces[2] + 'character(len=*),intent(in) :: dir' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        if self.any_non_primitive_high_dimension:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c.append(L)
        c.append(self.spaces[2] + "integer :: un" )
        c.append(self.spaces[2] + "un = open_to_read(dir,'primitives')" )
        c.append(self.spaces[2] + "call import_primitives(this,un)" )
        c.append(self.spaces[2] + "close(un)" )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_import_restart()])
        c.append(self.end_sub())
        return c

    def make_restart_dir_module(self):
        sig = 'make_restart_dir_' + self.get_name()
        # sig = 'make_restart_dir_'
        c = [self.full_sub_signature(sig,'this,dir')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'character(len=*),intent(in) :: dir' )
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        if self.any_non_primitive_high_dimension:
            for key in self.arg_objects:
                L = self.arg_objects[key].get_list_of_local_shape()
                if L: c.append(L)
        if suppress_all_warnings:
            c.append(self.spaces[2] + 'call suppress_warnings(this)' )
        c.append(self.spaces[2] + 'call make_dir_quiet(dir)' )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_make_restart_dir()])
        c.append(self.end_sub())
        return c

    def suppress_fortran_warnings_module(self):
        if suppress_all_warnings:
            sig = 'suppress_warnings_' + self.get_name()
            c = [self.full_sub_signature(sig,'this')]
            c.append(self.spaces[2] + self.implicitNone)
            c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
            c.append(self.spaces[2] + 'if (.false.) call print(this)' )
            c.append(self.end_sub())
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
