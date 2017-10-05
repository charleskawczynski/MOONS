       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module momentum_mod
       use IO_tools_mod
       use PCG_solver_SF_mod
       use PCG_solver_VF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use mesh_mod
       use string_mod
       use time_statistics_VF_mod
       implicit none

       private
       public :: momentum
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_momentum;          end interface
       interface delete;           module procedure delete_momentum;             end interface
       interface display;          module procedure display_momentum;            end interface
       interface display_short;    module procedure display_short_momentum;      end interface
       interface display;          module procedure display_wrap_momentum;       end interface
       interface print;            module procedure print_momentum;              end interface
       interface print_short;      module procedure print_short_momentum;        end interface
       interface export;           module procedure export_momentum;             end interface
       interface export_primitives;module procedure export_primitives_momentum;  end interface
       interface import;           module procedure import_momentum;             end interface
       interface export_structured;module procedure export_structured_D_momentum;end interface
       interface import_structured;module procedure import_structured_D_momentum;end interface
       interface import_primitives;module procedure import_primitives_momentum;  end interface
       interface export;           module procedure export_wrap_momentum;        end interface
       interface import;           module procedure import_wrap_momentum;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_momentum;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_momentum;        end interface
       interface suppress_warnings;module procedure suppress_warnings_momentum;  end interface

       type momentum
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(PCG_Solver_SF) :: PCG_P
         type(PCG_Solver_VF) :: PCG_U
         type(time_statistics_VF) :: TS
         type(SF) :: p
         type(SF) :: divU
         type(SF) :: temp_CC
         type(VF) :: U
         type(VF) :: Ustar
         type(VF) :: Unm1
         type(VF) :: U_CC
         type(VF) :: F
         type(VF) :: Fnm1
         type(VF) :: L
         type(VF) :: temp_F1
         type(VF) :: temp_F2
         type(VF) :: temp_F3
         type(VF) :: temp_E
         type(VF) :: temp_CC_VF
         type(TF) :: U_E
         type(TF) :: TF_CC
         type(TF) :: TF_CC_edge
       end type

       contains

       subroutine init_copy_momentum(this,that)
         implicit none
         type(momentum),intent(inout) :: this
         type(momentum),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%PCG_P,that%PCG_P)
         call init(this%PCG_U,that%PCG_U)
         call init(this%TS,that%TS)
         call init(this%p,that%p)
         call init(this%divU,that%divU)
         call init(this%temp_CC,that%temp_CC)
         call init(this%U,that%U)
         call init(this%Ustar,that%Ustar)
         call init(this%Unm1,that%Unm1)
         call init(this%U_CC,that%U_CC)
         call init(this%F,that%F)
         call init(this%Fnm1,that%Fnm1)
         call init(this%L,that%L)
         call init(this%temp_F1,that%temp_F1)
         call init(this%temp_F2,that%temp_F2)
         call init(this%temp_F3,that%temp_F3)
         call init(this%temp_E,that%temp_E)
         call init(this%temp_CC_VF,that%temp_CC_VF)
         call init(this%U_E,that%U_E)
         call init(this%TF_CC,that%TF_CC)
         call init(this%TF_CC_edge,that%TF_CC_edge)
       end subroutine

       subroutine delete_momentum(this)
         implicit none
         type(momentum),intent(inout) :: this
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%PCG_P)
         call delete(this%PCG_U)
         call delete(this%TS)
         call delete(this%p)
         call delete(this%divU)
         call delete(this%temp_CC)
         call delete(this%U)
         call delete(this%Ustar)
         call delete(this%Unm1)
         call delete(this%U_CC)
         call delete(this%F)
         call delete(this%Fnm1)
         call delete(this%L)
         call delete(this%temp_F1)
         call delete(this%temp_F2)
         call delete(this%temp_F3)
         call delete(this%temp_E)
         call delete(this%temp_CC_VF)
         call delete(this%U_E)
         call delete(this%TF_CC)
         call delete(this%TF_CC_edge)
       end subroutine

       subroutine display_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%PCG_P,un)
         call display(this%PCG_U,un)
         call display(this%TS,un)
         call display(this%p,un)
         call display(this%divU,un)
         call display(this%temp_CC,un)
         call display(this%U,un)
         call display(this%Ustar,un)
         call display(this%Unm1,un)
         call display(this%U_CC,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%temp_F3,un)
         call display(this%temp_E,un)
         call display(this%temp_CC_VF,un)
         call display(this%U_E,un)
         call display(this%TF_CC,un)
         call display(this%TF_CC_edge,un)
       end subroutine

       subroutine display_short_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%PCG_P,un)
         call display(this%PCG_U,un)
         call display(this%TS,un)
         call display(this%p,un)
         call display(this%divU,un)
         call display(this%temp_CC,un)
         call display(this%U,un)
         call display(this%Ustar,un)
         call display(this%Unm1,un)
         call display(this%U_CC,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%temp_F1,un)
         call display(this%temp_F2,un)
         call display(this%temp_F3,un)
         call display(this%temp_E,un)
         call display(this%temp_CC_VF,un)
         call display(this%U_E,un)
         call display(this%TF_CC,un)
         call display(this%TF_CC_edge,un)
       end subroutine

       subroutine display_wrap_momentum(this,dir,name)
         implicit none
         type(momentum),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_momentum(this)
         implicit none
         type(momentum),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_momentum(this)
         implicit none
         type(momentum),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
         call export(this%m,un)
         call export(this%PCG_P,un)
         call export(this%PCG_U,un)
         call export(this%TS,un)
         call export(this%p,un)
         call export(this%divU,un)
         call export(this%temp_CC,un)
         call export(this%U,un)
         call export(this%Ustar,un)
         call export(this%Unm1,un)
         call export(this%U_CC,un)
         call export(this%F,un)
         call export(this%Fnm1,un)
         call export(this%L,un)
         call export(this%temp_F1,un)
         call export(this%temp_F2,un)
         call export(this%temp_F3,un)
         call export(this%temp_E,un)
         call export(this%temp_CC_VF,un)
         call export(this%U_E,un)
         call export(this%TF_CC,un)
         call export(this%TF_CC_edge,un)
       end subroutine

       subroutine import_momentum(this,un)
         implicit none
         type(momentum),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
         call import(this%m,un)
         call import(this%PCG_P,un)
         call import(this%PCG_U,un)
         call import(this%TS,un)
         call import(this%p,un)
         call import(this%divU,un)
         call import(this%temp_CC,un)
         call import(this%U,un)
         call import(this%Ustar,un)
         call import(this%Unm1,un)
         call import(this%U_CC,un)
         call import(this%F,un)
         call import(this%Fnm1,un)
         call import(this%L,un)
         call import(this%temp_F1,un)
         call import(this%temp_F2,un)
         call import(this%temp_F3,un)
         call import(this%temp_E,un)
         call import(this%temp_CC_VF,un)
         call import(this%U_E,un)
         call import(this%TF_CC,un)
         call import(this%TF_CC_edge,un)
       end subroutine

       subroutine export_primitives_momentum(this,un)
         implicit none
         type(momentum),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
       end subroutine

       subroutine import_primitives_momentum(this,un)
         implicit none
         type(momentum),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%suppress_warning
       end subroutine

       subroutine export_wrap_momentum(this,dir,name)
         implicit none
         type(momentum),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_momentum(this,dir,name)
         implicit none
         type(momentum),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_momentum(this,dir)
         implicit none
         type(momentum),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call set_IO_dir(this%PCG_P,dir//'PCG_P'//fortran_PS)
         call set_IO_dir(this%PCG_U,dir//'PCG_U'//fortran_PS)
         call set_IO_dir(this%TS,dir//'TS'//fortran_PS)
         call set_IO_dir(this%p,dir//'p'//fortran_PS)
         call set_IO_dir(this%divU,dir//'divU'//fortran_PS)
         call set_IO_dir(this%temp_CC,dir//'temp_CC'//fortran_PS)
         call set_IO_dir(this%U,dir//'U'//fortran_PS)
         call set_IO_dir(this%Ustar,dir//'Ustar'//fortran_PS)
         call set_IO_dir(this%Unm1,dir//'Unm1'//fortran_PS)
         call set_IO_dir(this%U_CC,dir//'U_CC'//fortran_PS)
         call set_IO_dir(this%F,dir//'F'//fortran_PS)
         call set_IO_dir(this%Fnm1,dir//'Fnm1'//fortran_PS)
         call set_IO_dir(this%L,dir//'L'//fortran_PS)
         call set_IO_dir(this%temp_F1,dir//'temp_F1'//fortran_PS)
         call set_IO_dir(this%temp_F2,dir//'temp_F2'//fortran_PS)
         call set_IO_dir(this%temp_F3,dir//'temp_F3'//fortran_PS)
         call set_IO_dir(this%temp_E,dir//'temp_E'//fortran_PS)
         call set_IO_dir(this%temp_CC_VF,dir//'temp_CC_VF'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_momentum(this,dir)
         implicit none
         type(momentum),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_IO_dir(this%PCG_P,dir//'PCG_P'//fortran_PS)
         call make_IO_dir(this%PCG_U,dir//'PCG_U'//fortran_PS)
         call make_IO_dir(this%TS,dir//'TS'//fortran_PS)
         if (get_necessary_for_restart(this%p)) then
           call make_IO_dir(this%p,dir//'p'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divU)) then
           call make_IO_dir(this%divU,dir//'divU'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call make_IO_dir(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U)) then
           call make_IO_dir(this%U,dir//'U'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Ustar)) then
           call make_IO_dir(this%Ustar,dir//'Ustar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Unm1)) then
           call make_IO_dir(this%Unm1,dir//'Unm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_CC)) then
           call make_IO_dir(this%U_CC,dir//'U_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call make_IO_dir(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call make_IO_dir(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call make_IO_dir(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call make_IO_dir(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call make_IO_dir(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F3)) then
           call make_IO_dir(this%temp_F3,dir//'temp_F3'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call make_IO_dir(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call make_IO_dir(this%temp_CC_VF,dir//'temp_CC_VF'//fortran_PS)
         endif
       end subroutine

       subroutine export_structured_D_momentum(this,dir)
         implicit none
         type(momentum),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%PCG_P,dir//'PCG_P'//fortran_PS)
         call export_structured(this%PCG_U,dir//'PCG_U'//fortran_PS)
         call export_structured(this%TS,dir//'TS'//fortran_PS)
         if (get_necessary_for_restart(this%p)) then
           call export_structured(this%p,dir//'p'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divU)) then
           call export_structured(this%divU,dir//'divU'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call export_structured(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U)) then
           call export_structured(this%U,dir//'U'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Ustar)) then
           call export_structured(this%Ustar,dir//'Ustar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Unm1)) then
           call export_structured(this%Unm1,dir//'Unm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_CC)) then
           call export_structured(this%U_CC,dir//'U_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call export_structured(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call export_structured(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call export_structured(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call export_structured(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call export_structured(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F3)) then
           call export_structured(this%temp_F3,dir//'temp_F3'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call export_structured(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call export_structured(this%temp_CC_VF,&
           dir//'temp_CC_VF'//fortran_PS)
         endif
         close(un)
       end subroutine

       subroutine import_structured_D_momentum(this,dir)
         implicit none
         type(momentum),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call delete(this)
         call import_primitives(this,un)
         call import_structured(this%PCG_P,dir//'PCG_P'//fortran_PS)
         call import_structured(this%PCG_U,dir//'PCG_U'//fortran_PS)
         call import_structured(this%TS,dir//'TS'//fortran_PS)
         if (get_necessary_for_restart(this%p)) then
           call import_structured(this%p,dir//'p'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%divU)) then
           call import_structured(this%divU,dir//'divU'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC)) then
           call import_structured(this%temp_CC,dir//'temp_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U)) then
           call import_structured(this%U,dir//'U'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Ustar)) then
           call import_structured(this%Ustar,dir//'Ustar'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Unm1)) then
           call import_structured(this%Unm1,dir//'Unm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_CC)) then
           call import_structured(this%U_CC,dir//'U_CC'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%F)) then
           call import_structured(this%F,dir//'F'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%Fnm1)) then
           call import_structured(this%Fnm1,dir//'Fnm1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%L)) then
           call import_structured(this%L,dir//'L'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F1)) then
           call import_structured(this%temp_F1,dir//'temp_F1'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F2)) then
           call import_structured(this%temp_F2,dir//'temp_F2'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_F3)) then
           call import_structured(this%temp_F3,dir//'temp_F3'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_E)) then
           call import_structured(this%temp_E,dir//'temp_E'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%temp_CC_VF)) then
           call import_structured(this%temp_CC_VF,&
           dir//'temp_CC_VF'//fortran_PS)
         endif
         close(un)
       end subroutine

       subroutine suppress_warnings_momentum(this)
         implicit none
         type(momentum),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module