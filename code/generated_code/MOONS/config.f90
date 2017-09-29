       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module config_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use dir_tree_mod
       use export_now_mod
       use export_safe_mod
       use kill_switch_mod
       use refine_mesh_mod
       use sim_params_mod
       use stop_clock_mod
       use string_mod
       implicit none

       private
       public :: config
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_config;           end interface
       interface delete;           module procedure delete_config;              end interface
       interface display;          module procedure display_config;             end interface
       interface display_short;    module procedure display_short_config;       end interface
       interface display;          module procedure display_wrap_config;        end interface
       interface print;            module procedure print_config;               end interface
       interface print_short;      module procedure print_short_config;         end interface
       interface export;           module procedure export_config;              end interface
       interface export_primitives;module procedure export_primitives_config;   end interface
       interface import;           module procedure import_config;              end interface
       interface export_structured;module procedure export_structured_D_config; end interface
       interface import_structured;module procedure import_structured_D_config; end interface
       interface import_primitives;module procedure import_primitives_config;   end interface
       interface export;           module procedure export_wrap_config;         end interface
       interface import;           module procedure import_wrap_config;         end interface
       interface set_IO_dir;       module procedure set_IO_dir_config;          end interface
       interface make_IO_dir;      module procedure make_IO_dir_config;         end interface
       interface suppress_warnings;module procedure suppress_warnings_config;   end interface
       interface export;           module procedure export_DN_config;           end interface
       interface import;           module procedure import_DN_config;           end interface
       interface export_structured;module procedure export_structured_DN_config;end interface
       interface import_structured;module procedure import_structured_DN_config;end interface

       type config
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(string) :: dir_target
         type(stop_clock) :: sc
         type(export_now) :: EN
         type(export_safe) :: ES
         type(refine_mesh) :: RM
         type(kill_switch) :: KS
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_copy_config(this,that)
         implicit none
         type(config),intent(inout) :: this
         type(config),intent(in) :: that
         call delete(this)
         call init(this%DT,that%DT)
         call init(this%SP,that%SP)
         call init(this%dir_target,that%dir_target)
         call init(this%sc,that%sc)
         call init(this%EN,that%EN)
         call init(this%ES,that%ES)
         call init(this%RM,that%RM)
         call init(this%KS,that%KS)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_config(this)
         implicit none
         type(config),intent(inout) :: this
         call delete(this%DT)
         call delete(this%SP)
         call delete(this%dir_target)
         call delete(this%sc)
         call delete(this%EN)
         call delete(this%ES)
         call delete(this%RM)
         call delete(this%KS)
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%sc,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_short_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%sc,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_wrap_config(this,dir,name)
         implicit none
         type(config),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_config(this)
         implicit none
         type(config),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_config(this)
         implicit none
         type(config),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call export(this%DT,un)
         call export(this%SP,un)
         call export(this%dir_target,un)
         call export(this%sc,un)
         call export(this%EN,un)
         call export(this%ES,un)
         call export(this%RM,un)
         call export(this%KS,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_config(this,un)
         implicit none
         type(config),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%DT,un)
         call import(this%SP,un)
         call import(this%dir_target,un)
         call import(this%sc,un)
         call import(this%EN,un)
         call import(this%ES,un)
         call import(this%RM,un)
         call import(this%KS,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine export_primitives_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_primitives_config(this,un)
         implicit none
         type(config),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_wrap_config(this,dir,name)
         implicit none
         type(config),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_config(this,dir,name)
         implicit none
         type(config),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine export_DN_config(this)
         implicit none
         type(config),intent(in) :: this
         call export(this,str(this%dir),str(this%name))
       end subroutine

       subroutine import_DN_config(this)
         implicit none
         type(config),intent(inout) :: this
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

       subroutine export_structured_DN_config(this)
         implicit none
         type(config),intent(in) :: this
         integer :: un
         un = new_and_open(str(this%dir),'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%DT,str(this%dir)//'DT'//fortran_PS)
         call export_structured(this%SP,str(this%dir)//'SP'//fortran_PS)
         call export_structured(this%sc,str(this%dir)//'sc'//fortran_PS)
         call export_structured(this%EN,str(this%dir)//'EN'//fortran_PS)
         call export_structured(this%ES,str(this%dir)//'ES'//fortran_PS)
         call export_structured(this%RM,str(this%dir)//'RM'//fortran_PS)
         call export_structured(this%KS,str(this%dir)//'KS'//fortran_PS)
       end subroutine

       subroutine import_structured_DN_config(this)
         implicit none
         type(config),intent(inout) :: this
         integer :: un
         un = open_to_read(str(this%dir),'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%DT,str(this%dir)//'DT'//fortran_PS)
         call import_structured(this%SP,str(this%dir)//'SP'//fortran_PS)
         call import_structured(this%sc,str(this%dir)//'sc'//fortran_PS)
         call import_structured(this%EN,str(this%dir)//'EN'//fortran_PS)
         call import_structured(this%ES,str(this%dir)//'ES'//fortran_PS)
         call import_structured(this%RM,str(this%dir)//'RM'//fortran_PS)
         call import_structured(this%KS,str(this%dir)//'KS'//fortran_PS)
       end subroutine

       subroutine set_IO_dir_config(this,dir)
         implicit none
         type(config),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call set_IO_dir(this%DT,dir//'DT'//fortran_PS)
         call set_IO_dir(this%SP,dir//'SP'//fortran_PS)
         call set_IO_dir(this%sc,dir//'sc'//fortran_PS)
         call set_IO_dir(this%EN,dir//'EN'//fortran_PS)
         call set_IO_dir(this%ES,dir//'ES'//fortran_PS)
         call set_IO_dir(this%RM,dir//'RM'//fortran_PS)
         call set_IO_dir(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine make_IO_dir_config(this,dir)
         implicit none
         type(config),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call init(this%dir,dir)
         call init(this%name,'primitives')
         call make_IO_dir(this%DT,dir//'DT'//fortran_PS)
         call make_IO_dir(this%SP,dir//'SP'//fortran_PS)
         call make_IO_dir(this%sc,dir//'sc'//fortran_PS)
         call make_IO_dir(this%EN,dir//'EN'//fortran_PS)
         call make_IO_dir(this%ES,dir//'ES'//fortran_PS)
         call make_IO_dir(this%RM,dir//'RM'//fortran_PS)
         call make_IO_dir(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine export_structured_D_config(this,dir)
         implicit none
         type(config),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting config structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_structured(this%DT,dir//'DT'//fortran_PS)
         call export_structured(this%SP,dir//'SP'//fortran_PS)
         call export_structured(this%sc,dir//'sc'//fortran_PS)
         call export_structured(this%EN,dir//'EN'//fortran_PS)
         call export_structured(this%ES,dir//'ES'//fortran_PS)
         call export_structured(this%RM,dir//'RM'//fortran_PS)
         call export_structured(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine import_structured_D_config(this,dir)
         implicit none
         type(config),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing config structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_structured(this%DT,dir//'DT'//fortran_PS)
         call import_structured(this%SP,dir//'SP'//fortran_PS)
         call import_structured(this%sc,dir//'sc'//fortran_PS)
         call import_structured(this%EN,dir//'EN'//fortran_PS)
         call import_structured(this%ES,dir//'ES'//fortran_PS)
         call import_structured(this%RM,dir//'RM'//fortran_PS)
         call import_structured(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_config(this)
         implicit none
         type(config),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module