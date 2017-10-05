import code_formatting as CF

class logical_list:
  def __init__(self):
    self.string = False
    self.primitive = False
    self.object = False
    self.procedure = False
  def init(self):
    self.string = False
    self.primitive = False
    self.object = False
    self.procedure = False

class fortran_property:
  # Additional routines to consider:
  # compare
  # "" many routines for variable size
  # WARNING: insists copy-able. if (size(obj%a).lt.1) stop 'Error: object allocated but size<1 in init_copy in object.f90'
  # Add available preprocessor directives: ! Pre-processor directives: (_DEBUG_MESH_)

  def __init__(self):
    self.name = 'default_name'
    self.restart_dir_name = 'default_name'
    self.class_ = 'default_class'
    self.privacy = 'default_privacy'
    self.object_type = 'default_object_type'
    self.default_value = '0'
    # self.fortran_string_sep = '/'
    self.fortran_string_sep = 'fortran_PS'
    self.if_sufficient_size = 'if_sufficient_size'
    self.default_real = '0'
    self.do_loop_iter = []
    self.do_loop_iter_max = []
    self.spaces = []

    self.LL = logical_list()

  def set_do_loop_iter(self):
    self.do_loop_iter = 'i_'+self.name
    self.do_loop_iter_max = 's_'+self.name

  def get_list_of_local_iterators(self):
    L = []
    s = self.spaces[2]
    if self.dimension>1:
      if self.rank==1:
        L = L + [s+'integer :: '+self.do_loop_iter]
      else:
        for i in range(1,self.rank):
          L = L + [s+'integer :: '+self.do_loop_iter+'_'+str(i)]
    if self.object_type == 'primitive': L = []
    return L

  def get_list_of_local_shape(self):
    L = []
    s = self.spaces[2]
    if self.dimension>1:
      if self.rank==1:
        pass
        L = L + [s+'integer :: '+self.do_loop_iter_max]
      else:
        L = L + [s+'integer,dimension('+str(self.rank)+') :: '+self.do_loop_iter_max]
    if self.object_type == 'primitive' and not self.allocatable: L = []
    return L

  def set_spaces(self,spaces): self.spaces = spaces;

  def set_name(self,name): self.name = name;
  def get_name(self): return self.name;
  def set_default_value(self,default_value): self.default_value = default_value;
  def get_default_value(self): return self.default_value;
  def set_class(self,class_): self.class_ = class_;
  def get_class(self): return self.class_;
  def set_privacy(self,privacy): self.privacy = privacy;
  def get_privacy(self): return self.privacy;
  def set_object_type(self,object_type): self.object_type = object_type;
  def get_object_type(self): return self.object_type;

  def write_class_definition(self,all_private,all_public):
    if all_private or all_public: privacy_temp = ''
    else: privacy_temp = ','+self.privacy

    if self.LL.primitive:
      L = [self.class_+self.sig+privacy_temp+' :: '+self.name + self.assign_default_value]
    elif self.LL.object:
      L = ['type(' + self.class_ + ')'+self.sig+privacy_temp+' :: '+self.name]
    elif self.LL.procedure:
      L = ['procedure(' + self.class_ + '),pointer,nopass'+self.sig+privacy_temp+' :: '+self.name]
    return L

  def set_display_spaces(self,display_spaces): self.display_spaces = display_spaces

  def write_init_copy(self):
    L = []
    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + ['this%'+self.name+' = that%' + self.name]
      L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['this%'+self.name+' = that%' + self.name]
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + ['this%'+self.name+' = that%' + self.name]
      L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' = that%' + self.name]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' = that%' + self.name]

    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + [self.int_rank_shape_that]
      L = L + ['if ('+self.do_loop_iter_max+'.gt.0) then']
      L = L + ['allocate(this%'+self.name+'('+self.int_rank_list+'))']
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call init(this%'+self.name+'('+self.do_loop_iter+'),that%' + self.name + '('+self.do_loop_iter+'))']
      L = L + ['enddo']
      L = L + ['endif']
      L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + [self.int_rank_shape_that]
      L = L + ['if ('+self.do_loop_iter_max+'.gt.0) then']
      L = L + ['allocate(this%'+self.name+'('+self.do_loop_iter_max+'))']
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call init(this%'+self.name+'('+self.do_loop_iter+'),that%' + self.name + '('+self.do_loop_iter+'))']
      L = L + ['enddo']
      L = L + ['endif']
      L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + [self.int_rank_shape_that]
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call init(this%'+self.name+'('+self.do_loop_iter+'),that%' + self.name + '('+self.do_loop_iter+'))']
      L = L + ['enddo']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['call init(this%'+self.name+',that%' + self.name + ')']

    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['this%'+self.name+' => that%' + self.name]
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['this%'+self.name+' => that%' + self.name]
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' => that%' + self.name]
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' => that%' + self.name]
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' => that%' + self.name]
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_delete(self):
    L = []
    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      # L = L + ['this%'+self.name+' = ' + self.default_value] # Not really necessary
      L = L + ['if (allocated(this%'+self.name+')) then']
      L = L + ['deallocate(this%'+self.name+')']
      L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['this%'+self.name+' = ' + self.default_value]
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      # L = L + ['this%'+self.name+' = ' + self.default_value] # Not really necessary
      L = L + ['if (allocated(this%'+self.name+')) then']
      L = L + ['deallocate(this%'+self.name+')']
      L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' = ' + self.default_value]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['this%'+self.name+' = ' + self.default_value]

    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['if (allocated(this%'+self.name+')) then']
      L = L + [self.int_rank_shape_this]
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call delete(this%'+self.name+'('+self.do_loop_iter+'))']
      L = L + ['enddo']
      L = L + ['deallocate(this%'+self.name+')']
      L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + [self.int_rank_shape_this]
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call delete(this%'+self.name+'('+self.do_loop_iter+'))']
      L = L + ['enddo']
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['if (allocated(this%'+self.name+')) then']
      L = L + [self.int_rank_shape_this]
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call delete(this%'+self.name+'('+self.do_loop_iter+'))']
      L = L + ['enddo']
      L = L + ['deallocate(this%'+self.name+')']
      L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + [self.int_rank_shape_this]
      L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
      L = L + ['call delete(this%'+self.name+'('+self.do_loop_iter+'))']
      L = L + ['enddo']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['call delete(this%'+self.name+')']

    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['nullify(this%'+self.name+')']
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ['nullify(this%'+self.name+')']
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['nullify(this%'+self.name+')']
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ['nullify(this%'+self.name+')']
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ['nullify(this%'+self.name+')']
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_export(self,primitives_only,non_primitives_only):
    L = []
    p = "write(un,*) '"+self.name+" "+self.display_spaces+" = ';"
    # p = ""
    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      if not non_primitives_only:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + [p+'write(un,*) this%'  +self.name]
        L = L + ['endif']
        L = L + ['else']
        L = L + [self.export_zero_shape]
        L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + [p+'write(un,*) this%'  +self.name]
        L = L + ['endif']
        L = L + ['else']
        L = L + [self.export_zero_shape]
        L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + [p+'write(un,*) this%'  +self.name]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + [p+'write(un,*) this%'  +self.name]

    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [x for x in ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]]
        L = L + ['call export(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['else']
        L = L + [self.export_zero_shape]
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [x for x in ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]]
        L = L + ['call export(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + [x for x in ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]]
        L = L + ['call export(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['else']
        L = L + [self.export_zero_shape]
        L = L + ['endif']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [x for x in ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]]
        L = L + ['call export(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + ['call export(this%' + self.name + ',un)']

    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_import(self,primitives_only,non_primitives_only):
    L = []
    p = "read(un,*); "
    # p = ""
    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      if not non_primitives_only:
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.int_rank_list+'))']
        L = L + [p+'read(un,*) this%'  +self.name]
        L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
      if not non_primitives_only:
        L = L + [p+'read(un,*) this%'  +self.name]
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.do_loop_iter_max+'))']
        L = L + [p+'read(un,*) this%'  +self.name]
        L = L + ['endif']
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + [p+'read(un,*) this%'  +self.name]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      if not non_primitives_only:
        L = L + [p+'read(un,*) this%'  +self.name]

    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.int_rank_list+'))']
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call import(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + ['call import(this%' + self.name + ',un)']
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.do_loop_iter_max+'))']
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call import(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call import(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      if not primitives_only or non_primitives_only:
        L = L + ['call import(this%' + self.name+',un)']

    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)


  def write_export_wrap_specialized(self):
    L = []
    if     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ["call export(this%" + self.name + ",dir,name//'_" + self.name + "')"]
    else: raise NameError('Case not caught!')
    return CF.indent_lines(L)

  def write_import_wrap_specialized(self):
    L = []
    if     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ["call import(this%" + self.name + ",dir,name//'_" + self.name + "')"]
    else: raise NameError('Case not caught!')
    return CF.indent_lines(L)

  def write_display(self):
    L = []
    skip_display = False
    # skip_display = True

    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      if skip_display: pass
      else:
        L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
      if skip_display: pass
      else:
        L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      if skip_display: pass
      else:
        L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      if skip_display: pass
      else:
        L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      if skip_display: pass
      else:
        L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + ['call display' +  '(this%' + self.name + ',un)']
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_display_short(self):
    L = []

    if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
      pass
      # L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
      L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
      pass
      # L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
      L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
      L = L + ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]

    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
        L = L + ['endif']
    elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + ['call display' +  '(this%' + self.name+'('+self.do_loop_iter+'),un)']
        L = L + ['enddo']
    elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + ['call display' +  '(this%' + self.name + ',un)']
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
    elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
    elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
    else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_set_IO_dir(self,fun_name):
    L = []
    f_sep = self.fortran_string_sep
    f_name = "'"+self.name+"'"
    suffix_loop = '//'+'int2str('+self.do_loop_iter+')'
    f_name_loop = "dir"+"//'"+self.name+"_'"+suffix_loop+"//"+f_sep
    # f_name = "'"+self.restart_dir_name+"'"
    # f_name = "'"+f_sep+self.restart_dir_name+"'"
    f_call = 'call '+fun_name

    # if not self.class_=='string':
    if True:
      if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + [f_call +  "(this%" + self.name + ",dir"+"//"+f_name+"//"+f_sep+")"]
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
      else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_export_structured(self,directory):
    L = []
    f_sep = self.fortran_string_sep
    f_name = "'"+self.name+"'"
    suffix_loop = '//'+'int2str('+self.do_loop_iter+')'
    f_name_loop = directory+"//'"+self.name+"_'"+suffix_loop+"//"+f_sep
    f_call = 'call export_structured'

    # if not self.class_=='string':
    if True:
      if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['else']
        L = L + ['write(un,*) 0']
        L = L + ['endif']
        L = L + ['else']
        L = L + ['write(un,*) '+self.do_loop_iter_max]
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['else']
        L = L + ['write(un,*) 0']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + [self.export_rank_shape]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + [f_call +  "(this%" + self.name + ","+directory+"//"+f_name+"//"+f_sep+")"]
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
      else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_import_structured(self,directory):
    L = []
    f_sep = self.fortran_string_sep
    f_name = "'"+self.name+"'"
    suffix_loop = '//'+'int2str('+self.do_loop_iter+')'
    f_name_loop = directory+"//'"+self.name+"_'"+suffix_loop+"//"+f_sep
    f_call = 'call import_structured'

    # if not self.class_=='string':
    if True:
      if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.int_rank_list+'))']
        L = L + [self.int_rank_shape_this]
        L = L + [self.if_sufficient_size]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['endif']
        L = L + ['else']
        L = L + ['write(un,*) '+self.do_loop_iter_max]
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.import_rank_shape]
        L = L + [self.if_sufficient_size]
        L = L + ['allocate(this%'+self.name+'('+self.do_loop_iter_max+'))']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        f_name = f_name_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'),'+f_name+')']
        L = L + ['enddo']
      elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + [f_call +  "(this%" + self.name + ","+directory+"//"+f_name+"//"+f_sep+")"]
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
      else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_set_procedures(self):
    L = []
    f_sep = self.fortran_string_sep
    f_name = "'"+self.name+"'"
    suffix_loop = '//'+'int2str('+self.do_loop_iter+')'
    f_call = 'call set_procedures'

    if not self.class_=='string':
      if       self.LL.primitive and     self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and     self.rank>1:
        pass
      elif     self.LL.primitive and     self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and     self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.primitive and not self.allocatable and not self.dimension>1 and not self.rank>1:
        pass
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + [self.if_sufficient_size]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'))']
        L = L + ['enddo']
        L = L + ['endif']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and     self.rank>1:
        suffix = suffix_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'))']
        L = L + ['enddo']
      elif     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'))']
        L = L + ['enddo']
        L = L + ['endif']
      elif     self.LL.object    and not self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        # pass
        L = L + [self.int_rank_shape_this]
        L = L + ['do '+self.do_loop_iter+'=1,'+self.do_loop_iter_max]
        L = L + [f_call + '(this%' + self.name+'('+self.do_loop_iter+'))']
        L = L + ['enddo']
      elif     self.LL.object    and not self.allocatable and not self.dimension>1 and not self.rank>1:
        L = L + [f_call +  "(this%" + self.name + ")"]
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
      elif     self.LL.procedure and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
      elif     self.LL.procedure and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
      else: raise NameError('Case not caught!')

    return CF.indent_lines(L)

  def write_get_necessary_for_restart_SF(self):
    L = []
    f_sep = self.fortran_string_sep
    f_name = "'"+self.name+"'"
    suffix_loop = '//'+'int2str('+self.do_loop_iter+')'
    f_call = 'call set_procedures'

    if not self.class_=='string':
      if     self.LL.object    and     self.allocatable and     self.dimension>1 and not self.rank>1:
        suffix = suffix_loop
        # pass
        L = L + ['if (allocated(this%'+self.name+')) then']
        L = L + [self.int_rank_shape_this]
        L = L + ['L = all((/(get_necessary_for_restart(this%'+self.name+'('+self.do_loop_iter+')'+'),'+self.do_loop_iter+'=1,'+self.do_loop_iter_max+')/))']
        L = L + ['else']
        L = L + ['L = .false.']
        L = L + ['endif']
      else: raise NameError('Case not caught!')

    return CF.indent_lines(L)


  def set_default_primitives(self):
    if self.object_type=='primitive':
      self.LL = logical_list()
      self.LL.primitive = True

    if self.object_type=='object':
      self.LL = logical_list()
      self.LL.object = True

    if self.object_type=='procedure':
      self.LL = logical_list()
      self.LL.procedure = True

    primitive_list = ['integer','logical','character','real']
    if self.LL.primitive and 'integer' in self.class_.lower():
      self.default_value = '0'
    if self.LL.primitive and 'logical' in self.class_.lower():
      self.default_value = '.false.'
    if self.LL.primitive and 'character' in self.class_.lower():
      self.default_value = "' '"
    if self.LL.primitive and 'real' in self.class_.lower():
      self.default_value = self.default_real

  def set_default_real(self,default_real): self.default_real = default_real

  def print(self):
    print('--------------------------------- property')
    print('name        = '+str(self.name))
    print('class_      = '+str(self.class_))
    print('privacy     = '+str(self.privacy))
    print('object_type = '+str(self.object_type))
    print('---------------------------------')
    return

  def init_remaining(self,name,class_,privacy,allocatable = False,rank = 1,dimension = 1,procedure = False):
    self.name = name
    self.class_ = class_
    self.privacy = privacy
    # self.name = name.lower()
    # self.class_ = class_.lower()
    # self.privacy = privacy.lower()

    self.dimension = dimension
    self.procedure = procedure
    self.dimension_s = dimension
    self.restart_dir_name = class_
    self.rank = rank
    self.name_length = len(name)
    self.allocatable = allocatable
    self.do_loop_iter = 'i_'+self.name
    self.do_loop_iter_max = 's_'+self.name

    primitive_list = ['integer','logical','real','character']
    if any([x in class_  for x in primitive_list if not '_' in class_]):
      self.object_type = 'primitive'
    else:
      if procedure:
        self.object_type = 'procedure'
      else:
        self.object_type = 'object'
    self.set_default_primitives()

    if rank>1 and dimension<=1 and not allocatable: raise ValueError('rank>1 and dimension<=1')
    if allocatable and dimension<=1: raise ValueError('allocatable and dimension<=1')

    self.rank_deffered = (rank*':,')[:-1]
    if rank>1:
      self.int_rank_shape_that = self.do_loop_iter_max+' = shape(that%'+self.name+')'
      self.int_rank_shape_this = self.do_loop_iter_max+' = shape(this%'+self.name+')'
      self.import_rank_shape = 'read(un,*) '+self.do_loop_iter_max
      self.export_rank_shape = 'write(un,*) '+self.do_loop_iter_max
      self.export_zero_shape = 'write(un,*) 0'
      self.int_rank_list = ''.join([self.do_loop_iter_max+'('+str(x+1)+'),' for x in range(self.rank)])[:-1]
      self.if_sufficient_size = 'if (all((/'+''.join([self.do_loop_iter_max+'('+str(x+1)+').gt.0,' for x in range(self.rank)])[:-1]+'/))) then'
    else:
      self.int_rank_shape_that = self.do_loop_iter_max+' = size(that%'+self.name+')'
      self.int_rank_shape_this = self.do_loop_iter_max+' = size(this%'+self.name+')'
      self.import_rank_shape = 'read(un,*) '+self.do_loop_iter_max
      self.export_rank_shape = 'write(un,*) '+self.do_loop_iter_max
      self.export_zero_shape = 'write(un,*) 0'
      self.if_sufficient_size = 'if ('+self.do_loop_iter_max+'.gt.0) then'
      self.int_rank_list = ''

    if allocatable and dimension>1: self.dimension_s = self.rank_deffered
    else: self.dimension_s = str(dimension)

    if self.LL.primitive and 'character' in class_:
      if dimension>1 and allocatable:
        self.sig = '(len='+str(dimension)+')'+',dimension('+self.rank_deffered+'),allocatable'
        self.assign_default_value = ''
      elif dimension>1 and not allocatable:
        self.sig = '(len='+str(dimension)+')'
        self.assign_default_value = ' = '+self.default_value
      else:
        self.assign_default_value = ' = '+self.default_value
        self.sig = ''
    else:
      if dimension>1 and allocatable:
        self.sig = ',dimension('+self.rank_deffered+'),allocatable'
        self.assign_default_value = ''
      elif dimension>1 and not allocatable:
        self.sig = ',dimension('+str(dimension)+')'
        self.assign_default_value = ' = '+self.default_value
      else:
        self.assign_default_value = ' = '+self.default_value
        self.sig = ''

    return self
