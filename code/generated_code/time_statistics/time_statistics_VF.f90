       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_statistics_VF_mod
       use IO_tools_mod
       use TF_mod
       use VF_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use probe_mod
       use string_mod
       use time_statistics_params_mod
       implicit none

       private
       public :: time_statistics_VF
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_time_statistics_VF;        end interface
       interface delete;           module procedure delete_time_statistics_VF;           end interface
       interface display;          module procedure display_time_statistics_VF;          end interface
       interface display_short;    module procedure display_short_time_statistics_VF;    end interface
       interface display;          module procedure display_wrap_time_statistics_VF;     end interface
       interface print;            module procedure print_time_statistics_VF;            end interface
       interface print_short;      module procedure print_short_time_statistics_VF;      end interface
       interface export;           module procedure export_time_statistics_VF;           end interface
       interface export_primitives;module procedure export_primitives_time_statistics_VF;end interface
       interface export_restart;   module procedure export_restart_time_statistics_VF;   end interface
       interface import;           module procedure import_time_statistics_VF;           end interface
       interface import_restart;   module procedure import_restart_time_statistics_VF;   end interface
       interface import_primitives;module procedure import_primitives_time_statistics_VF;end interface
       interface export;           module procedure export_wrap_time_statistics_VF;      end interface
       interface import;           module procedure import_wrap_time_statistics_VF;      end interface
       interface make_restart_dir; module procedure make_restart_dir_time_statistics_VF; end interface
       interface suppress_warnings;module procedure suppress_warnings_time_statistics_VF;end interface
       interface export;           module procedure export_DN_time_statistics_VF;        end interface
       interface import;           module procedure import_DN_time_statistics_VF;        end interface

       type time_statistics_VF
         type(string) :: dir
         type(string) :: name
         type(VF) :: U_sum
         type(VF) :: U_ave
         type(probe) :: mean_energy
         type(VF) :: RMS
         type(TF) :: stresses
         type(TF) :: stresses_sum
         type(probe) :: L2_stresses
         type(time_statistics_params) :: TSP
       end type

       contains

       subroutine init_copy_time_statistics_VF(this,that)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         type(time_statistics_VF),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         call init(this%U_sum,that%U_sum)
         call init(this%U_ave,that%U_ave)
         call init(this%mean_energy,that%mean_energy)
         call init(this%RMS,that%RMS)
         call init(this%stresses,that%stresses)
         call init(this%stresses_sum,that%stresses_sum)
         call init(this%L2_stresses,that%L2_stresses)
         call init(this%TSP,that%TSP)
       end subroutine

       subroutine delete_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         call delete(this%U_sum)
         call delete(this%U_ave)
         call delete(this%mean_energy)
         call delete(this%RMS)
         call delete(this%stresses)
         call delete(this%stresses_sum)
         call delete(this%L2_stresses)
         call delete(this%TSP)
       end subroutine

       subroutine display_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%U_sum,un)
         call display(this%U_ave,un)
         call display(this%mean_energy,un)
         call display(this%RMS,un)
         call display(this%stresses,un)
         call display(this%stresses_sum,un)
         call display(this%L2_stresses,un)
         call display(this%TSP,un)
       end subroutine

       subroutine display_short_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%U_sum,un)
         call display(this%U_ave,un)
         call display(this%mean_energy,un)
         call display(this%RMS,un)
         call display(this%stresses,un)
         call display(this%stresses_sum,un)
         call display(this%L2_stresses,un)
         call display(this%TSP,un)
       end subroutine

       subroutine display_wrap_time_statistics_VF(this,dir,name)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         call export(this%U_sum,un)
         call export(this%U_ave,un)
         call export(this%mean_energy,un)
         call export(this%RMS,un)
         call export(this%stresses,un)
         call export(this%stresses_sum,un)
         call export(this%L2_stresses,un)
         call export(this%TSP,un)
       end subroutine

       subroutine import_primitives_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_time_statistics_VF(this,un)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         call import(this%U_sum,un)
         call import(this%U_ave,un)
         call import(this%mean_energy,un)
         call import(this%RMS,un)
         call import(this%stresses,un)
         call import(this%stresses_sum,un)
         call import(this%L2_stresses,un)
         call import(this%TSP,un)
       end subroutine

       subroutine export_restart_time_statistics_VF(this,dir)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%U_sum,dir//fortran_PS//'U_sum')
         call export_restart(this%U_ave,dir//fortran_PS//'U_ave')
         call export_restart(this%mean_energy,dir//fortran_PS//'mean_energy')
         call export_restart(this%RMS,dir//fortran_PS//'RMS')
         call export_restart(this%stresses,dir//fortran_PS//'stresses')
         call export_restart(this%stresses_sum,&
         dir//fortran_PS//'stresses_sum')
         call export_restart(this%L2_stresses,dir//fortran_PS//'L2_stresses')
         call export_restart(this%TSP,dir//fortran_PS//'TSP')
       end subroutine

       subroutine import_restart_time_statistics_VF(this,dir)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%U_sum,dir//fortran_PS//'U_sum')
         call import_restart(this%U_ave,dir//fortran_PS//'U_ave')
         call import_restart(this%mean_energy,dir//fortran_PS//'mean_energy')
         call import_restart(this%RMS,dir//fortran_PS//'RMS')
         call import_restart(this%stresses,dir//fortran_PS//'stresses')
         call import_restart(this%stresses_sum,&
         dir//fortran_PS//'stresses_sum')
         call import_restart(this%L2_stresses,dir//fortran_PS//'L2_stresses')
         call import_restart(this%TSP,dir//fortran_PS//'TSP')
       end subroutine

       subroutine export_wrap_time_statistics_VF(this,dir,name)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_time_statistics_VF(this,dir,name)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(inout) :: this
         type(string) :: dir,name
         integer :: un
         call init(dir,this%dir)
         call init(name,this%name)
         un = open_to_read(str(dir),str(name))
         call import(this,un)
         call delete(dir)
         call delete(name)
         close(un)
       end subroutine

       subroutine make_restart_dir_time_statistics_VF(this,dir)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%U_sum,dir//fortran_PS//'U_sum')
         call make_restart_dir(this%U_ave,dir//fortran_PS//'U_ave')
         call make_restart_dir(this%mean_energy,&
         dir//fortran_PS//'mean_energy')
         call make_restart_dir(this%RMS,dir//fortran_PS//'RMS')
         call make_restart_dir(this%stresses,dir//fortran_PS//'stresses')
         call make_restart_dir(this%stresses_sum,&
         dir//fortran_PS//'stresses_sum')
         call make_restart_dir(this%L2_stresses,&
         dir//fortran_PS//'L2_stresses')
         call make_restart_dir(this%TSP,dir//fortran_PS//'TSP')
       end subroutine

       subroutine suppress_warnings_time_statistics_VF(this)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module