       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module block_field_mod
       use IO_tools_mod
       use data_location_mod
       use boundary_conditions_mod
       use grid_field_mod
       use procedure_array_plane_op_mod
       implicit none

       private
       public :: block_field
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_block_field;    end interface
       interface delete;       module procedure delete_block_field;       end interface
       interface display;      module procedure display_block_field;      end interface
       interface display_short;module procedure display_short_block_field;end interface
       interface display;      module procedure display_wrap_block_field; end interface
       interface print;        module procedure print_block_field;        end interface
       interface print_short;  module procedure print_short_block_field;  end interface
       interface export;       module procedure export_block_field;       end interface
       interface import;       module procedure import_block_field;       end interface
       interface export;       module procedure export_wrap_block_field;  end interface
       interface import;       module procedure import_wrap_block_field;  end interface

       type block_field
         type(grid_field) :: GF
         type(boundary_conditions) :: BCs
         type(data_location) :: DL
         logical,dimension(3) :: many_cell_N_periodic = .false.
         logical,dimension(3) :: many_cell = .false.
         type(procedure_array_plane_op) :: PA_assign_ghost_XPeriodic
         type(procedure_array_plane_op) :: PA_assign_ghost_N_XPeriodic
         type(procedure_array_plane_op) :: PA_assign_wall_Dirichlet
         type(procedure_array_plane_op) :: PA_assign_wall_Periodic_single
         type(procedure_array_plane_op) :: PA_multiply_wall_Neumann
       end type

       contains

       subroutine init_copy_block_field(this,that)
         implicit none
         type(block_field),intent(inout) :: this
         type(block_field),intent(in) :: that
         call delete(this)
         call init(this%GF,that%GF)
         call init(this%BCs,that%BCs)
         call init(this%DL,that%DL)
         this%many_cell_N_periodic = that%many_cell_N_periodic
         this%many_cell = that%many_cell
         call init(this%PA_assign_ghost_XPeriodic,that%PA_assign_ghost_XPeriodic)
         call init(this%PA_assign_ghost_N_XPeriodic,that%PA_assign_ghost_N_XPeriodic)
         call init(this%PA_assign_wall_Dirichlet,that%PA_assign_wall_Dirichlet)
         call init(this%PA_assign_wall_Periodic_single,that%PA_assign_wall_Periodic_single)
         call init(this%PA_multiply_wall_Neumann,that%PA_multiply_wall_Neumann)
       end subroutine

       subroutine delete_block_field(this)
         implicit none
         type(block_field),intent(inout) :: this
         call delete(this%GF)
         call delete(this%BCs)
         call delete(this%DL)
         this%many_cell_N_periodic = .false.
         this%many_cell = .false.
         call delete(this%PA_assign_ghost_XPeriodic)
         call delete(this%PA_assign_ghost_N_XPeriodic)
         call delete(this%PA_assign_wall_Dirichlet)
         call delete(this%PA_assign_wall_Periodic_single)
         call delete(this%PA_multiply_wall_Neumann)
       end subroutine

       subroutine display_block_field(this,un)
         implicit none
         type(block_field),intent(in) :: this
         integer,intent(in) :: un
         call display(this%GF,un)
         call display(this%BCs,un)
         call display(this%DL,un)
         call display(this%PA_assign_ghost_XPeriodic,un)
         call display(this%PA_assign_ghost_N_XPeriodic,un)
         call display(this%PA_assign_wall_Dirichlet,un)
         call display(this%PA_assign_wall_Periodic_single,un)
         call display(this%PA_multiply_wall_Neumann,un)
       end subroutine

       subroutine display_short_block_field(this,un)
         implicit none
         type(block_field),intent(in) :: this
         integer,intent(in) :: un
         call display(this%GF,un)
         call display(this%BCs,un)
         call display(this%DL,un)
         write(un,*) 'many_cell_N_periodic           = ',this%many_cell_N_periodic
         write(un,*) 'many_cell                      = ',this%many_cell
         call display(this%PA_assign_ghost_XPeriodic,un)
         call display(this%PA_assign_ghost_N_XPeriodic,un)
         call display(this%PA_assign_wall_Dirichlet,un)
         call display(this%PA_assign_wall_Periodic_single,un)
         call display(this%PA_multiply_wall_Neumann,un)
       end subroutine

       subroutine print_block_field(this)
         implicit none
         type(block_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_block_field(this)
         implicit none
         type(block_field),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_block_field(this,un)
         implicit none
         type(block_field),intent(in) :: this
         integer,intent(in) :: un
         call export(this%GF,un)
         call export(this%BCs,un)
         call export(this%DL,un)
         write(un,*) 'many_cell_N_periodic            = ';write(un,*) this%many_cell_N_periodic
         write(un,*) 'many_cell                       = ';write(un,*) this%many_cell
         call export(this%PA_assign_ghost_XPeriodic,un)
         call export(this%PA_assign_ghost_N_XPeriodic,un)
         call export(this%PA_assign_wall_Dirichlet,un)
         call export(this%PA_assign_wall_Periodic_single,un)
         call export(this%PA_multiply_wall_Neumann,un)
       end subroutine

       subroutine import_block_field(this,un)
         implicit none
         type(block_field),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%GF,un)
         call import(this%BCs,un)
         call import(this%DL,un)
         read(un,*); read(un,*) this%many_cell_N_periodic
         read(un,*); read(un,*) this%many_cell
         call import(this%PA_assign_ghost_XPeriodic,un)
         call import(this%PA_assign_ghost_N_XPeriodic,un)
         call import(this%PA_assign_wall_Dirichlet,un)
         call import(this%PA_assign_wall_Periodic_single,un)
         call import(this%PA_multiply_wall_Neumann,un)
       end subroutine

       subroutine display_wrap_block_field(this,dir,name)
         implicit none
         type(block_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_block_field(this,dir,name)
         implicit none
         type(block_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_block_field(this,dir,name)
         implicit none
         type(block_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module