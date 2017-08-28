       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module equation_term_mod
       use current_precision_mod
       use IO_tools_mod
       implicit none

       private
       public :: equation_term
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_equation_term;           end interface
       interface delete; module procedure delete_equation_term;         end interface
       interface display;module procedure display_equation_term;        end interface
       interface display;module procedure display_wrapper_equation_term;end interface
       interface print;  module procedure print_equation_term;          end interface
       interface export; module procedure export_equation_term;         end interface
       interface import; module procedure import_equation_term;         end interface
       interface export; module procedure export_wrapper_equation_term; end interface
       interface import; module procedure import_wrapper_equation_term; end interface

       type equation_term
         logical :: add = .false.
         real(cp) :: scale = 0.0_cp
       end type

       contains

       subroutine init_equation_term(this,that)
         implicit none
         type(equation_term),intent(inout) :: this
         type(equation_term),intent(in) :: that
         call delete(this)
         this%add = that%add
         this%scale = that%scale
       end subroutine

       subroutine delete_equation_term(this)
         implicit none
         type(equation_term),intent(inout) :: this
         this%add = .false.
         this%scale = 0.0_cp
       end subroutine

       subroutine display_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- equation_term'
         write(un,*) 'add   = ',this%add
         write(un,*) 'scale = ',this%scale
       end subroutine

       subroutine print_equation_term(this)
         implicit none
         type(equation_term),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_equation_term(this,un)
         implicit none
         type(equation_term),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'add    = ';write(un,*) this%add
         write(un,*) 'scale  = ';write(un,*) this%scale
       end subroutine

       subroutine import_equation_term(this,un)
         implicit none
         type(equation_term),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%add
         read(un,*); read(un,*) this%scale
       end subroutine

       subroutine display_wrapper_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_equation_term(this,dir,name)
         implicit none
         type(equation_term),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module