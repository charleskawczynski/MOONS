       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module var_set_mod
       use IO_tools_mod
       use var_mod
       implicit none

       private
       public :: var_set
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_var_set;           end interface
       interface delete; module procedure delete_var_set;         end interface
       interface display;module procedure display_var_set;        end interface
       interface display;module procedure display_wrapper_var_set;end interface
       interface print;  module procedure print_var_set;          end interface
       interface export; module procedure export_var_set;         end interface
       interface import; module procedure import_var_set;         end interface
       interface export; module procedure export_wrapper_var_set; end interface
       interface import; module procedure import_wrapper_var_set; end interface

       type var_set
         type(var) :: t
         type(var) :: u
         type(var) :: p
         type(var) :: b
         type(var) :: b0
         type(var) :: phi
         type(var) :: rho
       end type

       contains

       subroutine init_var_set(this,that)
         implicit none
         type(var_set),intent(inout) :: this
         type(var_set),intent(in) :: that
         call delete(this)
         call init(this%t,that%t)
         call init(this%u,that%u)
         call init(this%p,that%p)
         call init(this%b,that%b)
         call init(this%b0,that%b0)
         call init(this%phi,that%phi)
         call init(this%rho,that%rho)
       end subroutine

       subroutine delete_var_set(this)
         implicit none
         type(var_set),intent(inout) :: this
         call delete(this%t)
         call delete(this%u)
         call delete(this%p)
         call delete(this%b)
         call delete(this%b0)
         call delete(this%phi)
         call delete(this%rho)
       end subroutine

       subroutine display_var_set(this,un)
         implicit none
         type(var_set),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- var_set'
         call display(this%t,un)
         call display(this%u,un)
         call display(this%p,un)
         call display(this%b,un)
         call display(this%b0,un)
         call display(this%phi,un)
         call display(this%rho,un)
       end subroutine

       subroutine print_var_set(this)
         implicit none
         type(var_set),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_var_set(this,un)
         implicit none
         type(var_set),intent(in) :: this
         integer,intent(in) :: un
         call export(this%t,un)
         call export(this%u,un)
         call export(this%p,un)
         call export(this%b,un)
         call export(this%b0,un)
         call export(this%phi,un)
         call export(this%rho,un)
       end subroutine

       subroutine import_var_set(this,un)
         implicit none
         type(var_set),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%t,un)
         call import(this%u,un)
         call import(this%p,un)
         call import(this%b,un)
         call import(this%b0,un)
         call import(this%phi,un)
         call import(this%rho,un)
       end subroutine

       subroutine display_wrapper_var_set(this,dir,name)
         implicit none
         type(var_set),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_var_set(this,dir,name)
         implicit none
         type(var_set),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_var_set(this,dir,name)
         implicit none
         type(var_set),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module