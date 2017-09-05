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
       public :: display_short,print_short

       interface init;         module procedure init_copy_in;    end interface
       interface delete;       module procedure delete_in;       end interface
       interface display;      module procedure display_in;      end interface
       interface display_short;module procedure display_short_in;end interface
       interface display;      module procedure display_wrap_in; end interface
       interface print;        module procedure print_in;        end interface
       interface print_short;  module procedure print_short_in;  end interface
       interface export;       module procedure export_in;       end interface
       interface import;       module procedure import_in;       end interface
       interface export;       module procedure export_wrap_in;  end interface
       interface import;       module procedure import_wrap_in;  end interface

       type induction_terms
         type(equation_term) :: advection
         type(equation_term) :: diffusion
         type(equation_term) :: diffusion_linear
         type(equation_term) :: unsteady_B0
         type(equation_term) :: current
         type(equation_term) :: B_applied
       end type

       contains

       subroutine init_copy_in(this,that)
         implicit none
         type(induction_terms),intent(inout) :: this
         type(induction_terms),intent(in) :: that
         call delete(this)
         call init(this%advection,that%advection)
         call init(this%diffusion,that%diffusion)
         call init(this%diffusion_linear,that%diffusion_linear)
         call init(this%unsteady_B0,that%unsteady_B0)
         call init(this%current,that%current)
         call init(this%B_applied,that%B_applied)
       end subroutine

       subroutine delete_in(this)
         implicit none
         type(induction_terms),intent(inout) :: this
         call delete(this%advection)
         call delete(this%diffusion)
         call delete(this%diffusion_linear)
         call delete(this%unsteady_B0)
         call delete(this%current)
         call delete(this%B_applied)
       end subroutine

       subroutine display_in(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- induction_terms'
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%unsteady_B0,un)
         call display(this%current,un)
         call display(this%B_applied,un)
       end subroutine

       subroutine display_short_in(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call display(this%advection,un)
         call display(this%diffusion,un)
         call display(this%diffusion_linear,un)
         call display(this%unsteady_B0,un)
         call display(this%current,un)
         call display(this%B_applied,un)
       end subroutine

       subroutine print_in(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_in(this)
         implicit none
         type(induction_terms),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_in(this,un)
         implicit none
         type(induction_terms),intent(in) :: this
         integer,intent(in) :: un
         call export(this%advection,un)
         call export(this%diffusion,un)
         call export(this%diffusion_linear,un)
         call export(this%unsteady_B0,un)
         call export(this%current,un)
         call export(this%B_applied,un)
       end subroutine

       subroutine import_in(this,un)
         implicit none
         type(induction_terms),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%advection,un)
         call import(this%diffusion,un)
         call import(this%diffusion_linear,un)
         call import(this%unsteady_B0,un)
         call import(this%current,un)
         call import(this%B_applied,un)
       end subroutine

       subroutine display_wrap_in(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_in(this,dir,name)
         implicit none
         type(induction_terms),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_in(this,dir,name)
         implicit none
         type(induction_terms),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module