       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module norms_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: norms
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_norms;           end interface
       interface delete; module procedure delete_norms;         end interface
       interface display;module procedure display_norms;        end interface
       interface display;module procedure display_wrapper_norms;end interface
       interface print;  module procedure print_norms;          end interface
       interface export; module procedure export_norms;         end interface
       interface import; module procedure import_norms;         end interface
       interface export; module procedure export_wrapper_norms; end interface
       interface import; module procedure import_wrapper_norms; end interface

       type norms
         real(cp) :: l1 = 0.0_cp
         real(cp) :: l2 = 0.0_cp
         real(cp) :: linf = 0.0_cp
       end type

       contains

       subroutine init_norms(this,that)
         implicit none
         type(norms),intent(inout) :: this
         type(norms),intent(in) :: that
         call delete(this)
         this%l1 = that%l1
         this%l2 = that%l2
         this%linf = that%linf
       end subroutine

       subroutine delete_norms(this)
         implicit none
         type(norms),intent(inout) :: this
         this%l1 = 0.0_cp
         this%l2 = 0.0_cp
         this%linf = 0.0_cp
       end subroutine

       subroutine display_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- norms'
         write(un,*) 'l1   = ',this%l1
         write(un,*) 'l2   = ',this%l2
         write(un,*) 'linf = ',this%linf
       end subroutine

       subroutine print_norms(this)
         implicit none
         type(norms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_norms(this,un)
         implicit none
         type(norms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'l1    = ';write(un,*) this%l1
         write(un,*) 'l2    = ';write(un,*) this%l2
         write(un,*) 'linf  = ';write(un,*) this%linf
       end subroutine

       subroutine import_norms(this,un)
         implicit none
         type(norms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%l1
         read(un,*); read(un,*) this%l2
         read(un,*); read(un,*) this%linf
       end subroutine

       subroutine display_wrapper_norms(this,dir,name)
         implicit none
         type(norms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_norms(this,dir,name)
         implicit none
         type(norms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_norms(this,dir,name)
         implicit none
         type(norms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module