       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module MOONS_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use dir_tree_mod
       use energy_mod
       use export_now_mod
       use export_safe_mod
       use induction_mod
       use kill_switch_mod
       use mesh_mod
       use momentum_mod
       use refine_mesh_mod
       use restart_file_mod
       use sim_params_mod
       use stop_clock_mod
       use string_mod
       implicit none

       private
       public :: MOONS
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_MOONS;        end interface
       interface delete;           module procedure delete_MOONS;           end interface
       interface display;          module procedure display_MOONS;          end interface
       interface display_short;    module procedure display_short_MOONS;    end interface
       interface display;          module procedure display_wrap_MOONS;     end interface
       interface print;            module procedure print_MOONS;            end interface
       interface print_short;      module procedure print_short_MOONS;      end interface
       interface export;           module procedure export_MOONS;           end interface
       interface export_primitives;module procedure export_primitives_MOONS;end interface
       interface export_restart;   module procedure export_restart_MOONS;   end interface
       interface import;           module procedure import_MOONS;           end interface
       interface import_restart;   module procedure import_restart_MOONS;   end interface
       interface import_primitives;module procedure import_primitives_MOONS;end interface
       interface export;           module procedure export_wrap_MOONS;      end interface
       interface import;           module procedure import_wrap_MOONS;      end interface
       interface make_restart_dir; module procedure make_restart_dir_MOONS; end interface
       interface suppress_warnings;module procedure suppress_warnings_MOONS;end interface

       type MOONS
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(string) :: dir_target
         type(mesh) :: m_temp
         type(stop_clock) :: sc
         type(restart_file) :: RF
         type(export_now) :: EN
         type(export_safe) :: ES
         type(refine_mesh) :: RM
         type(kill_switch) :: KS
       end type

       contains

       subroutine init_copy_MOONS(this,that)
         implicit none
         type(MOONS),intent(inout) :: this
         type(MOONS),intent(in) :: that
         call delete(this)
         call init(this%mom,that%mom)
         call init(this%ind,that%ind)
         call init(this%nrg,that%nrg)
         call init(this%DT,that%DT)
         call init(this%SP,that%SP)
         call init(this%dir_target,that%dir_target)
         call init(this%m_temp,that%m_temp)
         call init(this%sc,that%sc)
         call init(this%RF,that%RF)
         call init(this%EN,that%EN)
         call init(this%ES,that%ES)
         call init(this%RM,that%RM)
         call init(this%KS,that%KS)
       end subroutine

       subroutine delete_MOONS(this)
         implicit none
         type(MOONS),intent(inout) :: this
         call delete(this%mom)
         call delete(this%ind)
         call delete(this%nrg)
         call delete(this%DT)
         call delete(this%SP)
         call delete(this%dir_target)
         call delete(this%m_temp)
         call delete(this%sc)
         call delete(this%RF)
         call delete(this%EN)
         call delete(this%ES)
         call delete(this%RM)
         call delete(this%KS)
       end subroutine

       subroutine display_MOONS(this,un)
         implicit none
         type(MOONS),intent(in) :: this
         integer,intent(in) :: un
         call display(this%mom,un)
         call display(this%ind,un)
         call display(this%nrg,un)
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%m_temp,un)
         call display(this%sc,un)
         call display(this%RF,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
       end subroutine

       subroutine display_short_MOONS(this,un)
         implicit none
         type(MOONS),intent(in) :: this
         integer,intent(in) :: un
         call display(this%mom,un)
         call display(this%ind,un)
         call display(this%nrg,un)
         call display(this%DT,un)
         call display(this%SP,un)
         call display(this%dir_target,un)
         call display(this%m_temp,un)
         call display(this%sc,un)
         call display(this%RF,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
       end subroutine

       subroutine display_wrap_MOONS(this,dir,name)
         implicit none
         type(MOONS),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_MOONS(this)
         implicit none
         type(MOONS),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_MOONS(this)
         implicit none
         type(MOONS),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_MOONS(this,un)
         implicit none
         type(MOONS),intent(in) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine export_MOONS(this,un)
         implicit none
         type(MOONS),intent(in) :: this
         integer,intent(in) :: un
         call export(this%mom,un)
         call export(this%ind,un)
         call export(this%nrg,un)
         call export(this%DT,un)
         call export(this%SP,un)
         call export(this%dir_target,un)
         call export(this%m_temp,un)
         call export(this%sc,un)
         call export(this%RF,un)
         call export(this%EN,un)
         call export(this%ES,un)
         call export(this%RM,un)
         call export(this%KS,un)
       end subroutine

       subroutine import_primitives_MOONS(this,un)
         implicit none
         type(MOONS),intent(inout) :: this
         integer,intent(in) :: un
         integer :: un_suppress_warning
         un_suppress_warning = un
         call suppress_warnings(this)
       end subroutine

       subroutine import_MOONS(this,un)
         implicit none
         type(MOONS),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%mom,un)
         call import(this%ind,un)
         call import(this%nrg,un)
         call import(this%DT,un)
         call import(this%SP,un)
         call import(this%dir_target,un)
         call import(this%m_temp,un)
         call import(this%sc,un)
         call import(this%RF,un)
         call import(this%EN,un)
         call import(this%ES,un)
         call import(this%RM,un)
         call import(this%KS,un)
       end subroutine

       subroutine export_wrap_MOONS(this,dir,name)
         implicit none
         type(MOONS),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_MOONS(this,dir,name)
         implicit none
         type(MOONS),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_MOONS(this,dir)
         implicit none
         type(MOONS),intent(in) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         call make_restart_dir(this%mom,dir//fortran_PS//'mom')
         call make_restart_dir(this%ind,dir//fortran_PS//'ind')
         call make_restart_dir(this%nrg,dir//fortran_PS//'nrg')
         call make_restart_dir(this%DT,dir//fortran_PS//'DT')
         call make_restart_dir(this%SP,dir//fortran_PS//'SP')
         call make_restart_dir(this%m_temp,dir//fortran_PS//'m_temp')
         call make_restart_dir(this%sc,dir//fortran_PS//'sc')
         call make_restart_dir(this%RF,dir//fortran_PS//'RF')
         call make_restart_dir(this%EN,dir//fortran_PS//'EN')
         call make_restart_dir(this%ES,dir//fortran_PS//'ES')
         call make_restart_dir(this%RM,dir//fortran_PS//'RM')
         call make_restart_dir(this%KS,dir//fortran_PS//'KS')
       end subroutine

       subroutine export_restart_MOONS(this,dir)
         implicit none
         type(MOONS),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
         call export_restart(this%mom,dir//fortran_PS//'mom')
         call export_restart(this%ind,dir//fortran_PS//'ind')
         call export_restart(this%nrg,dir//fortran_PS//'nrg')
         call export_restart(this%DT,dir//fortran_PS//'DT')
         call export_restart(this%SP,dir//fortran_PS//'SP')
         call export_restart(this%m_temp,dir//fortran_PS//'m_temp')
         call export_restart(this%sc,dir//fortran_PS//'sc')
         call export_restart(this%RF,dir//fortran_PS//'RF')
         call export_restart(this%EN,dir//fortran_PS//'EN')
         call export_restart(this%ES,dir//fortran_PS//'ES')
         call export_restart(this%RM,dir//fortran_PS//'RM')
         call export_restart(this%KS,dir//fortran_PS//'KS')
       end subroutine

       subroutine import_restart_MOONS(this,dir)
         implicit none
         type(MOONS),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
         call import_restart(this%mom,dir//fortran_PS//'mom')
         call import_restart(this%ind,dir//fortran_PS//'ind')
         call import_restart(this%nrg,dir//fortran_PS//'nrg')
         call import_restart(this%DT,dir//fortran_PS//'DT')
         call import_restart(this%SP,dir//fortran_PS//'SP')
         call import_restart(this%m_temp,dir//fortran_PS//'m_temp')
         call import_restart(this%sc,dir//fortran_PS//'sc')
         call import_restart(this%RF,dir//fortran_PS//'RF')
         call import_restart(this%EN,dir//fortran_PS//'EN')
         call import_restart(this%ES,dir//fortran_PS//'ES')
         call import_restart(this%RM,dir//fortran_PS//'RM')
         call import_restart(this%KS,dir//fortran_PS//'KS')
       end subroutine

       subroutine suppress_warnings_MOONS(this)
         implicit none
         type(MOONS),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module