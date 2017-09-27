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
       use restart_file_mod
       use sim_params_mod
       use stop_clock_mod
       use string_mod
       implicit none

       private
       public :: config
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_config;        end interface
       interface delete;           module procedure delete_config;           end interface
       interface display;          module procedure display_config;          end interface
       interface display_short;    module procedure display_short_config;    end interface
       interface display;          module procedure display_wrap_config;     end interface
       interface print;            module procedure print_config;            end interface
       interface print_short;      module procedure print_short_config;      end interface
       interface export;           module procedure export_config;           end interface
       interface export_primitives;module procedure export_primitives_config;end interface
       interface export_restart;   module procedure export_restart_config;   end interface
       interface import;           module procedure import_config;           end interface
       interface import_restart;   module procedure import_restart_config;   end interface
       interface import_primitives;module procedure import_primitives_config;end interface
       interface export;           module procedure export_wrap_config;      end interface
       interface import;           module procedure import_wrap_config;      end interface
       interface make_restart_dir; module procedure make_restart_dir_config; end interface
       interface suppress_warnings;module procedure suppress_warnings_config;end interface

       type config
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(string) :: dir_target
         type(stop_clock) :: sc
         type(restart_file) :: RF
         type(export_now) :: EN
         type(export_safe) :: ES
         type(refine_mesh) :: RM
         type(kill_switch) :: KS
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
         call init(this%RF,that%RF)
         call init(this%EN,that%EN)
         call init(this%ES,that%ES)
         call init(this%RM,that%RM)
         call init(this%KS,that%KS)
       end subroutine

       subroutine delete_config(this)
         implicit none
         type(config),intent(inout) :: this
         call delete(this%DT)
         call delete(this%SP)
         call delete(this%dir_target)
         call delete(this%sc)
         call delete(this%RF)
         call delete(this%EN)
         call delete(this%ES)
         call delete(this%RM)
         call delete(this%KS)
       end subroutine

       subroutine display_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%sc,un)
         call display(this%RF,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
       end subroutine

       subroutine display_short_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%sc,un)
         call display(this%RF,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
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

       subroutine export_primitives_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_config(this,un)
         implicit none
         type(config),intent(in) :: this
         integer,intent(in) :: un
         call export(this%DT,un)
         call export(this%SP,un)
         call export(this%dir_target,un)
         call export(this%sc,un)
         call export(this%RF,un)
         call export(this%EN,un)
         call export(this%ES,un)
         call export(this%RM,un)
         call export(this%KS,un)
       end subroutine

       subroutine import_primitives_config(this,un)
         implicit none
         type(config),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
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
         call import(this%RF,un)
         call import(this%EN,un)
         call import(this%ES,un)
         call import(this%RM,un)
         call import(this%KS,un)
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
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_config(this,dir)
         implicit none
         type(config),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%DT,dir//'DT'//fortran_PS)
         call make_restart_dir(this%SP,dir//'SP'//fortran_PS)
         call make_restart_dir(this%sc,dir//'sc'//fortran_PS)
         call make_restart_dir(this%RF,dir//'RF'//fortran_PS)
         call make_restart_dir(this%EN,dir//'EN'//fortran_PS)
         call make_restart_dir(this%ES,dir//'ES'//fortran_PS)
         call make_restart_dir(this%RM,dir//'RM'//fortran_PS)
         call make_restart_dir(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine export_restart_config(this,dir)
         implicit none
         type(config),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%DT,dir//'DT'//fortran_PS)
         call export_restart(this%SP,dir//'SP'//fortran_PS)
         call export_restart(this%sc,dir//'sc'//fortran_PS)
         call export_restart(this%RF,dir//'RF'//fortran_PS)
         call export_restart(this%EN,dir//'EN'//fortran_PS)
         call export_restart(this%ES,dir//'ES'//fortran_PS)
         call export_restart(this%RM,dir//'RM'//fortran_PS)
         call export_restart(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine import_restart_config(this,dir)
         implicit none
         type(config),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%DT,dir//'DT'//fortran_PS)
         call import_restart(this%SP,dir//'SP'//fortran_PS)
         call import_restart(this%sc,dir//'sc'//fortran_PS)
         call import_restart(this%RF,dir//'RF'//fortran_PS)
         call import_restart(this%EN,dir//'EN'//fortran_PS)
         call import_restart(this%ES,dir//'ES'//fortran_PS)
         call import_restart(this%RM,dir//'RM'//fortran_PS)
         call import_restart(this%KS,dir//'KS'//fortran_PS)
       end subroutine

       subroutine suppress_warnings_config(this)
         implicit none
         type(config),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module