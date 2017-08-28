       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module moons_mod
       use IO_tools_mod
       use dir_tree_mod
       use energy_mod
       use induction_mod
       use mesh_mod
       use momentum_mod
       use restart_file_mod
       use sim_params_mod
       use string_mod
       implicit none

       private
       public :: moons
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_moons;           end interface
       interface delete; module procedure delete_moons;         end interface
       interface display;module procedure display_moons;        end interface
       interface display;module procedure display_wrapper_moons;end interface
       interface print;  module procedure print_moons;          end interface
       interface export; module procedure export_moons;         end interface
       interface import; module procedure import_moons;         end interface
       interface export; module procedure export_wrapper_moons; end interface
       interface import; module procedure import_wrapper_moons; end interface

       type moons
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         type(dir_tree) :: dt
         type(sim_params) :: sp
         type(restart_file) :: rf
         type(string) :: dir_target
         type(mesh) :: m_temp
       end type

       contains

       subroutine init_moons(this,that)
         implicit none
         type(moons),intent(inout) :: this
         type(moons),intent(in) :: that
         call delete(this)
         call init(this%mom,that%mom)
         call init(this%ind,that%ind)
         call init(this%nrg,that%nrg)
         call init(this%dt,that%dt)
         call init(this%sp,that%sp)
         call init(this%rf,that%rf)
         call init(this%dir_target,that%dir_target)
         call init(this%m_temp,that%m_temp)
       end subroutine

       subroutine delete_moons(this)
         implicit none
         type(moons),intent(inout) :: this
         call delete(this%mom)
         call delete(this%ind)
         call delete(this%nrg)
         call delete(this%dt)
         call delete(this%sp)
         call delete(this%rf)
         call delete(this%dir_target)
         call delete(this%m_temp)
       end subroutine

       subroutine display_moons(this,un)
         implicit none
         type(moons),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- moons'
         call display(this%mom,un)
         call display(this%ind,un)
         call display(this%nrg,un)
         call display(this%dt,un)
         call display(this%sp,un)
         call display(this%rf,un)
         call display(this%dir_target,un)
         call display(this%m_temp,un)
       end subroutine

       subroutine print_moons(this)
         implicit none
         type(moons),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_moons(this,un)
         implicit none
         type(moons),intent(in) :: this
         integer,intent(in) :: un
         call export(this%mom,un)
         call export(this%ind,un)
         call export(this%nrg,un)
         call export(this%dt,un)
         call export(this%sp,un)
         call export(this%rf,un)
         call export(this%dir_target,un)
         call export(this%m_temp,un)
       end subroutine

       subroutine import_moons(this,un)
         implicit none
         type(moons),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%mom,un)
         call import(this%ind,un)
         call import(this%nrg,un)
         call import(this%dt,un)
         call import(this%sp,un)
         call import(this%rf,un)
         call import(this%dir_target,un)
         call import(this%m_temp,un)
       end subroutine

       subroutine display_wrapper_moons(this,dir,name)
         implicit none
         type(moons),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_moons(this,dir,name)
         implicit none
         type(moons),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_moons(this,dir,name)
         implicit none
         type(moons),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module