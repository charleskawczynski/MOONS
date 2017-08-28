       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module block_field_mod
       use IO_tools_mod
       use boundary_conditions_mod
       use data_location_mod
       use grid_field_mod
       use procedure_array_plane_op_mod
       implicit none

       private
       public :: block_field
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_block_field;           end interface
       interface delete; module procedure delete_block_field;         end interface
       interface display;module procedure display_block_field;        end interface
       interface display;module procedure display_wrapper_block_field;end interface
       interface print;  module procedure print_block_field;          end interface
       interface export; module procedure export_block_field;         end interface
       interface import; module procedure import_block_field;         end interface
       interface export; module procedure export_wrapper_block_field; end interface
       interface import; module procedure import_wrapper_block_field; end interface

       type block_field
         type(grid_field) :: gf
         type(boundary_conditions) :: bcs
         type(data_location) :: dl
         logical,dimension(3) :: many_cell_n_periodic = .false.
         logical,dimension(3) :: many_cell = .false.
         type(procedure_array_plane_op) :: pa_assign_ghost_xperiodic
         type(procedure_array_plane_op) :: pa_assign_ghost_n_xperiodic
         type(procedure_array_plane_op) :: pa_assign_wall_dirichlet
         type(procedure_array_plane_op) :: pa_assign_wall_periodic_single
         type(procedure_array_plane_op) :: pa_multiply_wall_neumann
       end type

       contains

       subroutine init_block_field(this,that)
         implicit none
         type(block_field),intent(inout) :: this
         type(block_field),intent(in) :: that
         call delete(this)
         call init(this%gf,that%gf)
         call init(this%bcs,that%bcs)
         call init(this%dl,that%dl)
         this%many_cell_n_periodic = that%many_cell_n_periodic
         this%many_cell = that%many_cell
         call init(this%pa_assign_ghost_xperiodic,that%pa_assign_ghost_xperiodic)
         call init(this%pa_assign_ghost_n_xperiodic,that%pa_assign_ghost_n_xperiodic)
         call init(this%pa_assign_wall_dirichlet,that%pa_assign_wall_dirichlet)
         call init(this%pa_assign_wall_periodic_single,that%pa_assign_wall_periodic_single)
         call init(this%pa_multiply_wall_neumann,that%pa_multiply_wall_neumann)
       end subroutine

       subroutine delete_block_field(this)
         implicit none
         type(block_field),intent(inout) :: this
         call delete(this%gf)
         call delete(this%bcs)
         call delete(this%dl)
         this%many_cell_n_periodic = .false.
         this%many_cell = .false.
         call delete(this%pa_assign_ghost_xperiodic)
         call delete(this%pa_assign_ghost_n_xperiodic)
         call delete(this%pa_assign_wall_dirichlet)
         call delete(this%pa_assign_wall_periodic_single)
         call delete(this%pa_multiply_wall_neumann)
       end subroutine

       subroutine display_block_field(this,un)
         implicit none
         type(block_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- block_field'
         call display(this%gf,un)
         call display(this%bcs,un)
         call display(this%dl,un)
         write(un,*) 'many_cell_n_periodic           = ',this%many_cell_n_periodic
         write(un,*) 'many_cell                      = ',this%many_cell
         call display(this%pa_assign_ghost_xperiodic,un)
         call display(this%pa_assign_ghost_n_xperiodic,un)
         call display(this%pa_assign_wall_dirichlet,un)
         call display(this%pa_assign_wall_periodic_single,un)
         call display(this%pa_multiply_wall_neumann,un)
       end subroutine

       subroutine print_block_field(this)
         implicit none
         type(block_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_block_field(this,un)
         implicit none
         type(block_field),intent(in) :: this
         integer,intent(in) :: un
         call export(this%gf,un)
         call export(this%bcs,un)
         call export(this%dl,un)
         write(un,*) 'many_cell_n_periodic            = ';write(un,*) this%many_cell_n_periodic
         write(un,*) 'many_cell                       = ';write(un,*) this%many_cell
         call export(this%pa_assign_ghost_xperiodic,un)
         call export(this%pa_assign_ghost_n_xperiodic,un)
         call export(this%pa_assign_wall_dirichlet,un)
         call export(this%pa_assign_wall_periodic_single,un)
         call export(this%pa_multiply_wall_neumann,un)
       end subroutine

       subroutine import_block_field(this,un)
         implicit none
         type(block_field),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%gf,un)
         call import(this%bcs,un)
         call import(this%dl,un)
         read(un,*); read(un,*) this%many_cell_n_periodic
         read(un,*); read(un,*) this%many_cell
         call import(this%pa_assign_ghost_xperiodic,un)
         call import(this%pa_assign_ghost_n_xperiodic,un)
         call import(this%pa_assign_wall_dirichlet,un)
         call import(this%pa_assign_wall_periodic_single,un)
         call import(this%pa_multiply_wall_neumann,un)
       end subroutine

       subroutine display_wrapper_block_field(this,dir,name)
         implicit none
         type(block_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_block_field(this,dir,name)
         implicit none
         type(block_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_block_field(this,dir,name)
         implicit none
         type(block_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module