       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module grid_field_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: grid_field
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_grid_field;           end interface
       interface delete; module procedure delete_grid_field;         end interface
       interface display;module procedure display_grid_field;        end interface
       interface display;module procedure display_wrapper_grid_field;end interface
       interface print;  module procedure print_grid_field;          end interface
       interface export; module procedure export_grid_field;         end interface
       interface import; module procedure import_grid_field;         end interface
       interface export; module procedure export_wrapper_grid_field; end interface
       interface import; module procedure import_wrapper_grid_field; end interface

       type grid_field
         real(cp),dimension(:,:,:),allocatable :: f
         integer,dimension(3) :: s = 0
         integer :: s_1d = 0
       end type

       contains

       subroutine init_grid_field(this,that)
         implicit none
         type(grid_field),intent(inout) :: this
         type(grid_field),intent(in) :: that
         call delete(this)
         this%f = that%f
         this%s = that%s
         this%s_1d = that%s_1d
       end subroutine

       subroutine delete_grid_field(this)
         implicit none
         type(grid_field),intent(inout) :: this
         this%f = 0.0_cp
         deallocate(this%f)
         this%s = 0
         this%s_1d = 0
       end subroutine

       subroutine display_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- grid_field'
         write(un,*) 'f    = ',this%f
         write(un,*) 's    = ',this%s
         write(un,*) 's_1d = ',this%s_1d
       end subroutine

       subroutine print_grid_field(this)
         implicit none
         type(grid_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         if (allocated(this%f)) then
           s_f = shape(this%f)
           write(un,*) s_f
           write(un,*) 'f     = ';write(un,*) this%f
         endif
         write(un,*) 's     = ';write(un,*) this%s
         write(un,*) 's_1d  = ';write(un,*) this%s_1d
       end subroutine

       subroutine import_grid_field(this,un)
         implicit none
         type(grid_field),intent(inout) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         call delete(this)
         read(un,*) s_f
         allocate(this%f(s_f(1),s_f(2),s_f(3)))
         read(un,*); read(un,*) this%f
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%s_1d
       end subroutine

       subroutine display_wrapper_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module