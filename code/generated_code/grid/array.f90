       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module array_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: array
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_array;           end interface
       interface delete; module procedure delete_array;         end interface
       interface display;module procedure display_array;        end interface
       interface display;module procedure display_wrapper_array;end interface
       interface print;  module procedure print_array;          end interface
       interface export; module procedure export_array;         end interface
       interface import; module procedure import_array;         end interface
       interface export; module procedure export_wrapper_array; end interface
       interface import; module procedure import_wrapper_array; end interface

       type array
         real(cp),dimension(:),allocatable :: f
         integer :: n = 0
       end type

       contains

       subroutine init_array(this,that)
         implicit none
         type(array),intent(inout) :: this
         type(array),intent(in) :: that
         call delete(this)
         this%f = that%f
         this%n = that%n
       end subroutine

       subroutine delete_array(this)
         implicit none
         type(array),intent(inout) :: this
         this%f = 0.0_cp
         deallocate(this%f)
         this%n = 0
       end subroutine

       subroutine display_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- array'
         write(un,*) 'f = ',this%f
         write(un,*) 'n = ',this%n
       end subroutine

       subroutine print_array(this)
         implicit none
         type(array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         integer :: s_f
         if (allocated(this%f)) then
           s_f = size(this%f)
           write(un,*) s_f
           write(un,*) 'f  = ';write(un,*) this%f
         endif
         write(un,*) 'n  = ';write(un,*) this%n
       end subroutine

       subroutine import_array(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         integer :: s_f
         call delete(this)
         read(un,*) s_f
         allocate(this%f(s_f))
         read(un,*); read(un,*) this%f
         read(un,*); read(un,*) this%n
       end subroutine

       subroutine display_wrapper_array(this,dir,name)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_array(this,dir,name)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_array(this,dir,name)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module