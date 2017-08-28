       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module induction_terms_mod
       use IO_tools_mod
       use equation_term_mod
       implicit none

       private
       public :: induction_terms
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_induction_terms;           end interface
       interface delete; module procedure delete_induction_terms;         end interface
       interface display;module procedure display_induction_terms;        end interface
       interface display;module procedure display_wrapper_induction_terms;end interface
       interface print;  module procedure print_induction_terms;          end interface
       interface export; module procedure export_induction_terms;         end interface
       interface import; module procedure import_induction_terms;         end interface
       interface export; module procedure export_wrapper_induction_terms; end interface
       interface import; module procedure import_wrapper_induction_terms; end interface

       type induction_terms
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: unsteady_b0
         type(equation_term) :: current
         type(equation_term) :: b_applied
       end type

       contains

       subroutine init_induction_terms(this,that)
         implicit none
         type(induction_terms),intent(inout) :: this
         type(induction_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%unsteady_b0,that%unsteady_b0)
         call init(this%current,that%current)
         call init(this%b_applied,that%b_applied)
       end subroutine

       subroutine delete_induction_terms(this)
         implicit none
         type(induction_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%unsteady_b0)
         call delete(this%current)
         call delete(this%b_applied)
       end subroutine

       subroutine display_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- induction_terms'
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%unsteady_b0,un)
         call display(this%current,un)
         call display(this%b_applied,un)
       end subroutine

       subroutine print_induction_terms(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%unsteady_b0,un)
         call export(this%current,un)
         call export(this%b_applied,un)
       end subroutine

       subroutine import_induction_terms(this,un)
         implicit none
         type(induction_terms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%unsteady_b0,un)
         call import(this%current,un)
         call import(this%b_applied,un)
       end subroutine

       subroutine display_wrapper_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_induction_terms(this,dir,name)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module