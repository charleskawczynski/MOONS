       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module energy_mod
       use IO_tools_mod
       use PCG_solver_SF_mod
       use SF_mod
       use TF_mod
       use VF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use mesh_mod
       use mesh_domain_mod
       use probe_mod
       use string_mod
       implicit none

       private
       public :: energy
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_energy;        end interface
       interface delete;           module procedure delete_energy;           end interface
       interface display;          module procedure display_energy;          end interface
       interface display_short;    module procedure display_short_energy;    end interface
       interface display;          module procedure display_wrap_energy;     end interface
       interface print;            module procedure print_energy;            end interface
       interface print_short;      module procedure print_short_energy;      end interface
       interface export;           module procedure export_energy;           end interface
       interface export_primitives;module procedure export_primitives_energy;end interface
       interface export_restart;   module procedure export_restart_energy;   end interface
       interface import;           module procedure import_energy;           end interface
       interface import_restart;   module procedure import_restart_energy;   end interface
       interface import_primitives;module procedure import_primitives_energy;end interface
       interface export;           module procedure export_wrap_energy;      end interface
       interface import;           module procedure import_wrap_energy;      end interface
       interface make_restart_dir; module procedure make_restart_dir_energy; end interface
       interface suppress_warnings;module procedure suppress_warnings_energy;end interface

       type energy
         logical :: suppress_warning = .false.
         type(mesh) :: m
         type(PCG_Solver_SF) :: PCG_T
         type(SF) :: T
         type(SF) :: Tnm1
         type(SF) :: temp_CC1
         type(SF) :: temp_CC2
         type(SF) :: F
         type(SF) :: Fnm1
         type(SF) :: L
         type(SF) :: divQ
         type(SF) :: Q_source
         type(VF) :: temp_F
         type(VF) :: k
         type(VF) :: U_F
         type(VF) :: U_CC
         type(VF) :: gravity
         type(VF) :: temp_CC1_VF
         type(VF) :: temp_CC2_VF
         type(TF) :: temp_CC_TF
         type(TF) :: temp_F_TF
         type(probe) :: probe_divQ
         type(mesh_domain) :: MD
       end type

       contains

       subroutine init_copy_energy(this,that)
         implicit none
         type(energy),intent(inout) :: this
         type(energy),intent(in) :: that
         call delete(this)
         this%suppress_warning = that%suppress_warning
         call init(this%m,that%m)
         call init(this%PCG_T,that%PCG_T)
         call init(this%T,that%T)
         call init(this%Tnm1,that%Tnm1)
         call init(this%temp_CC1,that%temp_CC1)
         call init(this%temp_CC2,that%temp_CC2)
         call init(this%F,that%F)
         call init(this%Fnm1,that%Fnm1)
         call init(this%L,that%L)
         call init(this%divQ,that%divQ)
         call init(this%Q_source,that%Q_source)
         call init(this%temp_F,that%temp_F)
         call init(this%k,that%k)
         call init(this%U_F,that%U_F)
         call init(this%U_CC,that%U_CC)
         call init(this%gravity,that%gravity)
         call init(this%temp_CC1_VF,that%temp_CC1_VF)
         call init(this%temp_CC2_VF,that%temp_CC2_VF)
         call init(this%temp_CC_TF,that%temp_CC_TF)
         call init(this%temp_F_TF,that%temp_F_TF)
         call init(this%probe_divQ,that%probe_divQ)
         call init(this%MD,that%MD)
       end subroutine

       subroutine delete_energy(this)
         implicit none
         type(energy),intent(inout) :: this
         this%suppress_warning = .false.
         call delete(this%m)
         call delete(this%PCG_T)
         call delete(this%T)
         call delete(this%Tnm1)
         call delete(this%temp_CC1)
         call delete(this%temp_CC2)
         call delete(this%F)
         call delete(this%Fnm1)
         call delete(this%L)
         call delete(this%divQ)
         call delete(this%Q_source)
         call delete(this%temp_F)
         call delete(this%k)
         call delete(this%U_F)
         call delete(this%U_CC)
         call delete(this%gravity)
         call delete(this%temp_CC1_VF)
         call delete(this%temp_CC2_VF)
         call delete(this%temp_CC_TF)
         call delete(this%temp_F_TF)
         call delete(this%probe_divQ)
         call delete(this%MD)
       end subroutine

       subroutine display_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%PCG_T,un)
         call display(this%T,un)
         call display(this%Tnm1,un)
         call display(this%temp_CC1,un)
         call display(this%temp_CC2,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%divQ,un)
         call display(this%Q_source,un)
         call display(this%temp_F,un)
         call display(this%k,un)
         call display(this%U_F,un)
         call display(this%U_CC,un)
         call display(this%gravity,un)
         call display(this%temp_CC1_VF,un)
         call display(this%temp_CC2_VF,un)
         call display(this%temp_CC_TF,un)
         call display(this%temp_F_TF,un)
         call display(this%probe_divQ,un)
         call display(this%MD,un)
       end subroutine

       subroutine display_short_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning = ',this%suppress_warning
         call display(this%m,un)
         call display(this%PCG_T,un)
         call display(this%T,un)
         call display(this%Tnm1,un)
         call display(this%temp_CC1,un)
         call display(this%temp_CC2,un)
         call display(this%F,un)
         call display(this%Fnm1,un)
         call display(this%L,un)
         call display(this%divQ,un)
         call display(this%Q_source,un)
         call display(this%temp_F,un)
         call display(this%k,un)
         call display(this%U_F,un)
         call display(this%U_CC,un)
         call display(this%gravity,un)
         call display(this%temp_CC1_VF,un)
         call display(this%temp_CC2_VF,un)
         call display(this%temp_CC_TF,un)
         call display(this%temp_F_TF,un)
         call display(this%probe_divQ,un)
         call display(this%MD,un)
       end subroutine

       subroutine display_wrap_energy(this,dir,name)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_energy(this)
         implicit none
         type(energy),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_energy(this)
         implicit none
         type(energy),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
       end subroutine

       subroutine export_energy(this,un)
         implicit none
         type(energy),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'suppress_warning  = ';write(un,*) this%suppress_warning
         call export(this%m,un)
         call export(this%PCG_T,un)
         call export(this%T,un)
         call export(this%Tnm1,un)
         call export(this%temp_CC1,un)
         call export(this%temp_CC2,un)
         call export(this%F,un)
         call export(this%Fnm1,un)
         call export(this%L,un)
         call export(this%divQ,un)
         call export(this%Q_source,un)
         call export(this%temp_F,un)
         call export(this%k,un)
         call export(this%U_F,un)
         call export(this%U_CC,un)
         call export(this%gravity,un)
         call export(this%temp_CC1_VF,un)
         call export(this%temp_CC2_VF,un)
         call export(this%temp_CC_TF,un)
         call export(this%temp_F_TF,un)
         call export(this%probe_divQ,un)
         call export(this%MD,un)
       end subroutine

       subroutine import_primitives_energy(this,un)
         implicit none
         type(energy),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%suppress_warning
       end subroutine

       subroutine import_energy(this,un)
         implicit none
         type(energy),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%suppress_warning
         call import(this%m,un)
         call import(this%PCG_T,un)
         call import(this%T,un)
         call import(this%Tnm1,un)
         call import(this%temp_CC1,un)
         call import(this%temp_CC2,un)
         call import(this%F,un)
         call import(this%Fnm1,un)
         call import(this%L,un)
         call import(this%divQ,un)
         call import(this%Q_source,un)
         call import(this%temp_F,un)
         call import(this%k,un)
         call import(this%U_F,un)
         call import(this%U_CC,un)
         call import(this%gravity,un)
         call import(this%temp_CC1_VF,un)
         call import(this%temp_CC2_VF,un)
         call import(this%temp_CC_TF,un)
         call import(this%temp_F_TF,un)
         call import(this%probe_divQ,un)
         call import(this%MD,un)
       end subroutine

       subroutine export_wrap_energy(this,dir,name)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_energy(this,dir,name)
         implicit none
         type(energy),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_energy(this,dir)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%m,dir//'m'//fortran_PS)
         call make_restart_dir(this%PCG_T,dir//'PCG_T'//fortran_PS)
         call make_restart_dir(this%T,dir//'T'//fortran_PS)
         call make_restart_dir(this%Tnm1,dir//'Tnm1'//fortran_PS)
         call make_restart_dir(this%temp_CC1,dir//'temp_CC1'//fortran_PS)
         call make_restart_dir(this%temp_CC2,dir//'temp_CC2'//fortran_PS)
         call make_restart_dir(this%F,dir//'F'//fortran_PS)
         call make_restart_dir(this%Fnm1,dir//'Fnm1'//fortran_PS)
         call make_restart_dir(this%L,dir//'L'//fortran_PS)
         call make_restart_dir(this%divQ,dir//'divQ'//fortran_PS)
         call make_restart_dir(this%Q_source,dir//'Q_source'//fortran_PS)
         call make_restart_dir(this%temp_F,dir//'temp_F'//fortran_PS)
         call make_restart_dir(this%k,dir//'k'//fortran_PS)
         call make_restart_dir(this%U_F,dir//'U_F'//fortran_PS)
         call make_restart_dir(this%U_CC,dir//'U_CC'//fortran_PS)
         call make_restart_dir(this%gravity,dir//'gravity'//fortran_PS)
         call make_restart_dir(this%temp_CC1_VF,&
         dir//'temp_CC1_VF'//fortran_PS)
         call make_restart_dir(this%temp_CC2_VF,&
         dir//'temp_CC2_VF'//fortran_PS)
         call make_restart_dir(this%temp_CC_TF,dir//'temp_CC_TF'//fortran_PS)
         call make_restart_dir(this%temp_F_TF,dir//'temp_F_TF'//fortran_PS)
         call make_restart_dir(this%probe_divQ,dir//'probe_divQ'//fortran_PS)
         call make_restart_dir(this%MD,dir//'MD'//fortran_PS)
       end subroutine

       subroutine export_restart_energy(this,dir)
         implicit none
         type(energy),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%m,dir//'m'//fortran_PS)
         call export_restart(this%PCG_T,dir//'PCG_T'//fortran_PS)
         call export_restart(this%T,dir//'T'//fortran_PS)
         call export_restart(this%Tnm1,dir//'Tnm1'//fortran_PS)
         call export_restart(this%temp_CC1,dir//'temp_CC1'//fortran_PS)
         call export_restart(this%temp_CC2,dir//'temp_CC2'//fortran_PS)
         call export_restart(this%F,dir//'F'//fortran_PS)
         call export_restart(this%Fnm1,dir//'Fnm1'//fortran_PS)
         call export_restart(this%L,dir//'L'//fortran_PS)
         call export_restart(this%divQ,dir//'divQ'//fortran_PS)
         call export_restart(this%Q_source,dir//'Q_source'//fortran_PS)
         call export_restart(this%temp_F,dir//'temp_F'//fortran_PS)
         call export_restart(this%k,dir//'k'//fortran_PS)
         call export_restart(this%U_F,dir//'U_F'//fortran_PS)
         call export_restart(this%U_CC,dir//'U_CC'//fortran_PS)
         call export_restart(this%gravity,dir//'gravity'//fortran_PS)
         call export_restart(this%temp_CC1_VF,dir//'temp_CC1_VF'//fortran_PS)
         call export_restart(this%temp_CC2_VF,dir//'temp_CC2_VF'//fortran_PS)
         call export_restart(this%temp_CC_TF,dir//'temp_CC_TF'//fortran_PS)
         call export_restart(this%temp_F_TF,dir//'temp_F_TF'//fortran_PS)
         call export_restart(this%probe_divQ,dir//'probe_divQ'//fortran_PS)
         call export_restart(this%MD,dir//'MD'//fortran_PS)
       end subroutine

       subroutine import_restart_energy(this,dir)
         implicit none
         type(energy),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%m,dir//'m'//fortran_PS)
         call import_restart(this%PCG_T,dir//'PCG_T'//fortran_PS)
         call import_restart(this%T,dir//'T'//fortran_PS)
         call import_restart(this%Tnm1,dir//'Tnm1'//fortran_PS)
         call import_restart(this%temp_CC1,dir//'temp_CC1'//fortran_PS)
         call import_restart(this%temp_CC2,dir//'temp_CC2'//fortran_PS)
         call import_restart(this%F,dir//'F'//fortran_PS)
         call import_restart(this%Fnm1,dir//'Fnm1'//fortran_PS)
         call import_restart(this%L,dir//'L'//fortran_PS)
         call import_restart(this%divQ,dir//'divQ'//fortran_PS)
         call import_restart(this%Q_source,dir//'Q_source'//fortran_PS)
         call import_restart(this%temp_F,dir//'temp_F'//fortran_PS)
         call import_restart(this%k,dir//'k'//fortran_PS)
         call import_restart(this%U_F,dir//'U_F'//fortran_PS)
         call import_restart(this%U_CC,dir//'U_CC'//fortran_PS)
         call import_restart(this%gravity,dir//'gravity'//fortran_PS)
         call import_restart(this%temp_CC1_VF,dir//'temp_CC1_VF'//fortran_PS)
         call import_restart(this%temp_CC2_VF,dir//'temp_CC2_VF'//fortran_PS)
         call import_restart(this%temp_CC_TF,dir//'temp_CC_TF'//fortran_PS)
         call import_restart(this%temp_F_TF,dir//'temp_F_TF'//fortran_PS)
         call import_restart(this%probe_divQ,dir//'probe_divQ'//fortran_PS)
         call import_restart(this%MD,dir//'MD'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_energy(this)
         implicit none
         type(energy),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module