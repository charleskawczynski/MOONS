       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module time_statistics_SF_mod
       use probe_mod
       use time_statistics_params_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use SF_mod
       use dir_manip_mod
       implicit none

       private
       public :: time_statistics_SF
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings,export,import,&
       export_structured,import_structured

       interface init;                   module procedure init_copy_time_statistics_SF;              end interface
       interface delete;                 module procedure delete_time_statistics_SF;                 end interface
       interface display;                module procedure display_time_statistics_SF;                end interface
       interface display_short;          module procedure display_short_time_statistics_SF;          end interface
       interface display;                module procedure display_wrap_time_statistics_SF;           end interface
       interface print;                  module procedure print_time_statistics_SF;                  end interface
       interface print_short;            module procedure print_short_time_statistics_SF;            end interface
       interface export;                 module procedure export_time_statistics_SF;                 end interface
       interface export_primitives;      module procedure export_primitives_time_statistics_SF;      end interface
       interface import;                 module procedure import_time_statistics_SF;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_time_statistics_SF;end interface
       interface export_structured;      module procedure export_structured_D_time_statistics_SF;    end interface
       interface import_structured;      module procedure import_structured_D_time_statistics_SF;    end interface
       interface import_primitives;      module procedure import_primitives_time_statistics_SF;      end interface
       interface export;                 module procedure export_wrap_time_statistics_SF;            end interface
       interface import;                 module procedure import_wrap_time_statistics_SF;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_time_statistics_SF;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_time_statistics_SF;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_time_statistics_SF;      end interface
       interface export;                 module procedure export_DN_time_statistics_SF;              end interface
       interface import;                 module procedure import_DN_time_statistics_SF;              end interface
       interface export_structured;      module procedure export_structured_DN_time_statistics_SF;   end interface
       interface import_structured;      module procedure import_structured_DN_time_statistics_SF;   end interface

       type time_statistics_SF
         type(string) :: dir
         type(string) :: name
         type(SF) :: U_sum
         type(SF) :: U_ave
         type(probe) :: mean_energy
         type(SF) :: RMS
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

       subroutine display_wrap_time_statistics_SF(this,dir,name)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
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

       subroutine export_primitives_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_time_statistics_SF(this,un)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
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
         call export(this,un)
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

       subroutine export_structured_DN_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         call export_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call export_structured(this%name,str(this%dir)//'name'//fortran_PS)
         call export_structured(this%U_sum,&
         str(this%dir)//'U_sum'//fortran_PS)
         call export_structured(this%U_ave,&
         str(this%dir)//'U_ave'//fortran_PS)
         call export_structured(this%mean_energy,&
         str(this%dir)//'mean_energy'//fortran_PS)
         call export_structured(this%RMS,str(this%dir)//'RMS'//fortran_PS)
         call export_structured(this%TSP,str(this%dir)//'TSP'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_DN_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         call import_structured(this%dir,str(this%dir)//'dir'//fortran_PS)
         call import_structured(this%name,str(this%dir)//'name'//fortran_PS)
         call import_structured(this%U_sum,&
         str(this%dir)//'U_sum'//fortran_PS)
         call import_structured(this%U_ave,&
         str(this%dir)//'U_ave'//fortran_PS)
         call import_structured(this%mean_energy,&
         str(this%dir)//'mean_energy'//fortran_PS)
         call import_structured(this%RMS,str(this%dir)//'RMS'//fortran_PS)
         call import_structured(this%TSP,str(this%dir)//'TSP'//fortran_PS)
         close(un)
       end subroutine

       subroutine set_IO_dir_time_statistics_SF(this,dir)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call set_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call set_IO_dir(this%name,dir//'name'//fortran_PS)
         call set_IO_dir(this%U_sum,dir//'U_sum'//fortran_PS)
         call set_IO_dir(this%U_ave,dir//'U_ave'//fortran_PS)
         call set_IO_dir(this%mean_energy,dir//'mean_energy'//fortran_PS)
         call set_IO_dir(this%RMS,dir//'RMS'//fortran_PS)
         call set_IO_dir(this%TSP,dir//'TSP'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_time_statistics_SF(this,dir)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call make_IO_dir(this%dir,dir//'dir'//fortran_PS)
         call make_IO_dir(this%name,dir//'name'//fortran_PS)
         if (get_necessary_for_restart(this%U_sum)) then
           call make_IO_dir(this%U_sum,dir//'U_sum'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_ave)) then
           call make_IO_dir(this%U_ave,dir//'U_ave'//fortran_PS)
         endif
         call make_IO_dir(this%mean_energy,dir//'mean_energy'//fortran_PS)
         if (get_necessary_for_restart(this%RMS)) then
           call make_IO_dir(this%RMS,dir//'RMS'//fortran_PS)
         endif
         call make_IO_dir(this%TSP,dir//'TSP'//fortran_PS)
       end subroutine

       subroutine export_folder_structure_time_statistics_SF(this,dir)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         call export_structured(this%dir,dir//'dir'//fortran_PS)
         call export_structured(this%name,dir//'name'//fortran_PS)
         if (get_necessary_for_restart(this%U_sum)) then
           call export_structured(this%U_sum,dir//'U_sum'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_ave)) then
           call export_structured(this%U_ave,dir//'U_ave'//fortran_PS)
         endif
         call export_structured(this%mean_energy,&
         dir//'mean_energy'//fortran_PS)
         if (get_necessary_for_restart(this%RMS)) then
           call export_structured(this%RMS,dir//'RMS'//fortran_PS)
         endif
         call export_structured(this%TSP,dir//'TSP'//fortran_PS)
       end subroutine

       subroutine export_structured_D_time_statistics_SF(this,dir)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         call export_structured(this%dir,dir//'dir'//fortran_PS)
         call export_structured(this%name,dir//'name'//fortran_PS)
         if (get_necessary_for_restart(this%U_sum)) then
           call export_structured(this%U_sum,dir//'U_sum'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_ave)) then
           call export_structured(this%U_ave,dir//'U_ave'//fortran_PS)
         endif
         call export_structured(this%mean_energy,&
         dir//'mean_energy'//fortran_PS)
         if (get_necessary_for_restart(this%RMS)) then
           call export_structured(this%RMS,dir//'RMS'//fortran_PS)
         endif
         call export_structured(this%TSP,dir//'TSP'//fortran_PS)
         close(un)
       end subroutine

       subroutine import_structured_D_time_statistics_SF(this,dir)
         implicit none
         type(time_statistics_SF),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         call import_structured(this%dir,dir//'dir'//fortran_PS)
         call import_structured(this%name,dir//'name'//fortran_PS)
         if (get_necessary_for_restart(this%U_sum)) then
           call import_structured(this%U_sum,dir//'U_sum'//fortran_PS)
         endif
         if (get_necessary_for_restart(this%U_ave)) then
           call import_structured(this%U_ave,dir//'U_ave'//fortran_PS)
         endif
         call import_structured(this%mean_energy,&
         dir//'mean_energy'//fortran_PS)
         if (get_necessary_for_restart(this%RMS)) then
           call import_structured(this%RMS,dir//'RMS'//fortran_PS)
         endif
         call import_structured(this%TSP,dir//'TSP'//fortran_PS)
         close(un)
       end subroutine

       subroutine suppress_warnings_time_statistics_SF(this)
         implicit none
         type(time_statistics_SF),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module