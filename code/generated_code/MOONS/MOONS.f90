       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module MOONS_mod
       use IO_tools_mod
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

       interface init;         module procedure init_copy_MOONS;    end interface
       interface delete;       module procedure delete_MOONS;       end interface
       interface display;      module procedure display_MOONS;      end interface
       interface display_short;module procedure display_short_MOONS;end interface
       interface display;      module procedure display_wrap_MOONS; end interface
       interface print;        module procedure print_MOONS;        end interface
       interface print_short;  module procedure print_short_MOONS;  end interface
       interface export;       module procedure export_MOONS;       end interface
       interface import;       module procedure import_MOONS;       end interface
       interface export;       module procedure export_wrap_MOONS;  end interface
       interface import;       module procedure import_wrap_MOONS;  end interface

       type MOONS
         type(momentum) :: mom
         type(induction) :: ind
         type(energy) :: nrg
         type(dir_tree) :: DT
         type(sim_params) :: SP
         type(string) :: dir_target
         type(mesh) :: m_temp
         type(stop_clock) :: sc
         logical :: fresh_restart_file = .false.
         logical :: matrix_visualization = .false.
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
         this%fresh_restart_file = that%fresh_restart_file
         this%matrix_visualization = that%matrix_visualization
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
         this%fresh_restart_file = .false.
         this%matrix_visualization = .false.
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
         write(un,*) 'fresh_restart_file   = ',this%fresh_restart_file
         write(un,*) 'matrix_visualization = ',this%matrix_visualization
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
         write(un,*) 'fresh_restart_file   = ',this%fresh_restart_file
         write(un,*) 'matrix_visualization = ',this%matrix_visualization
         call display(this%RF,un)
         call display(this%EN,un)
         call display(this%ES,un)
         call display(this%RM,un)
         call display(this%KS,un)
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
         write(un,*) 'fresh_restart_file    = ';write(un,*) this%fresh_restart_file
         write(un,*) 'matrix_visualization  = ';write(un,*) this%matrix_visualization
         call export(this%RF,un)
         call export(this%EN,un)
         call export(this%ES,un)
         call export(this%RM,un)
         call export(this%KS,un)
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
         read(un,*); read(un,*) this%fresh_restart_file
         read(un,*); read(un,*) this%matrix_visualization
         call import(this%RF,un)
         call import(this%EN,un)
         call import(this%ES,un)
         call import(this%RM,un)
         call import(this%KS,un)
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

       end module