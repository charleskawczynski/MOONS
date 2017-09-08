       module time_statistics_mod
       use IO_tools_mod
       use string_mod
       use VF_mod
       use time_statistics_params_mod
       use probe_mod
       use TF_mod
       use SF_mod

       public :: time_statistics_SF
       public :: init,delete,display,print,export,import
       public :: time_statistics_VF
       public :: display_short,print_short

       interface init;         module procedure init_copy_time_statistics_SF;    end interface
       interface delete;       module procedure delete_time_statistics_SF;       end interface
       interface display;      module procedure display_time_statistics_SF;      end interface
       interface display_short;module procedure display_short_time_statistics_SF;end interface
       interface display;      module procedure display_wrap_time_statistics_SF; end interface
       interface print;        module procedure print_time_statistics_SF;        end interface
       interface print_short;  module procedure print_short_time_statistics_SF;  end interface
       interface export;       module procedure export_time_statistics_SF;       end interface
       interface import;       module procedure import_time_statistics_SF;       end interface
       interface export;       module procedure export_wrap_time_statistics_SF;  end interface
       interface import;       module procedure import_wrap_time_statistics_SF;  end interface
       interface export;       module procedure export_DN_time_statistics_SF;    end interface
       interface import;       module procedure import_DN_time_statistics_SF;    end interface
       interface init;         module procedure init_copy_time_statistics_VF;    end interface
       interface delete;       module procedure delete_time_statistics_VF;       end interface
       interface display;      module procedure display_time_statistics_VF;      end interface
       interface display_short;module procedure display_short_time_statistics_VF;end interface
       interface display;      module procedure display_wrap_time_statistics_VF; end interface
       interface print;        module procedure print_time_statistics_VF;        end interface
       interface print_short;  module procedure print_short_time_statistics_VF;  end interface
       interface export;       module procedure export_time_statistics_VF;       end interface
       interface import;       module procedure import_time_statistics_VF;       end interface
       interface export;       module procedure export_wrap_time_statistics_VF;  end interface
       interface import;       module procedure import_wrap_time_statistics_VF;  end interface
       interface export;       module procedure export_DN_time_statistics_VF;    end interface
       interface import;       module procedure import_DN_time_statistics_VF;    end interface

       type time_statistics_SF
         type(string) :: dir
         type(string) :: name
         type(SF) :: U_sum
         type(SF) :: U_ave
         type(probe) :: mean_energy
         type(SF) :: RMS
         type(time_statistics_params) :: TSP
       end type

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

       subroutine init_copy_time_statistics_SF(this,that)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         type(time_statistics_SF),intent(in) :: that
         call delete(this)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
         call init(this%U_sum,that%U_sum)
         call init(this%U_ave,that%U_ave)
         call init(this%mean_energy,that%mean_energy)
         call init(this%RMS,that%RMS)
         call init(this%TSP,that%TSP)
       end subroutine

       subroutine delete_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         call delete(this%dir)
         call delete(this%name)
         call delete(this%U_sum)
         call delete(this%U_ave)
         call delete(this%mean_energy)
         call delete(this%RMS)
         call delete(this%TSP)
       end subroutine

       subroutine display_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%U_sum,un)
         call display(this%U_ave,un)
         call display(this%mean_energy,un)
         call display(this%RMS,un)
         call display(this%TSP,un)
       end subroutine

       subroutine display_short_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         integer,intent(in) :: un
         call display(this%dir,un)
         call display(this%name,un)
         call display(this%U_sum,un)
         call display(this%U_ave,un)
         call display(this%mean_energy,un)
         call display(this%RMS,un)
         call display(this%TSP,un)
       end subroutine

       subroutine print_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         integer,intent(in) :: un
         call export(this%dir,un)
         call export(this%name,un)
         call export(this%U_sum,un)
         call export(this%U_ave,un)
         call export(this%mean_energy,un)
         call export(this%RMS,un)
         call export(this%TSP,un)
       end subroutine

       subroutine import_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%dir,un)
         call import(this%name,un)
         call import(this%U_sum,un)
         call import(this%U_ave,un)
         call import(this%mean_energy,un)
         call import(this%RMS,un)
         call import(this%TSP,un)
       end subroutine

       subroutine display_wrap_time_statistics_SF(this,dir,name)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_time_statistics_SF(this,dir,name)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_time_statistics_SF(this,dir,name)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine export_DN_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
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

       subroutine display_wrap_time_statistics_VF(this,dir,name)
         implicit none
         type(time_statistics_VF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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

       end module