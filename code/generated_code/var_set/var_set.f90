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
       public :: display_short,print_short

       interface init;         module procedure init_copy_var_set;    end interface
       interface delete;       module procedure delete_var_set;       end interface
       interface display;      module procedure display_var_set;      end interface
       interface display_short;module procedure display_short_var_set;end interface
       interface display;      module procedure display_wrap_var_set; end interface
       interface print;        module procedure print_var_set;        end interface
       interface print_short;  module procedure print_short_var_set;  end interface
       interface export;       module procedure export_var_set;       end interface
       interface import;       module procedure import_var_set;       end interface
       interface export;       module procedure export_wrap_var_set;  end interface
       interface import;       module procedure import_wrap_var_set;  end interface

       type var_set
         type(var) :: T
         type(var) :: U
         type(var) :: p
         type(var) :: B
         type(var) :: B0
         type(var) :: phi
         type(var) :: rho
       end type

       contains

       subroutine init_copy_var_set(this,that)
         implicit none
         type(var_set),intent(inout) :: this
         type(var_set),intent(in) :: that
         call delete(this)
         call init(this%T,that%T)
         call init(this%U,that%U)
         call init(this%p,that%p)
         call init(this%B,that%B)
         call init(this%B0,that%B0)
         call init(this%phi,that%phi)
         call init(this%rho,that%rho)
       end subroutine

       subroutine delete_var_set(this)
         implicit none
         type(var_set),intent(inout) :: this
         call delete(this%T)
         call delete(this%U)
         call delete(this%p)
         call delete(this%B)
         call delete(this%B0)
         call delete(this%phi)
         call delete(this%rho)
       end subroutine

       subroutine display_var_set(this,un)
         implicit none
         type(var_set),intent(in) :: this
         integer,intent(in) :: un
         call display(this%T,un)
         call display(this%U,un)
         call display(this%p,un)
         call display(this%B,un)
         call display(this%B0,un)
         call display(this%phi,un)
         call display(this%rho,un)
       end subroutine

       subroutine display_short_var_set(this,un)
         implicit none
         type(var_set),intent(in) :: this
         integer,intent(in) :: un
         call display(this%T,un)
         call display(this%U,un)
         call display(this%p,un)
         call display(this%B,un)
         call display(this%B0,un)
         call display(this%phi,un)
         call display(this%rho,un)
       end subroutine

       subroutine print_var_set(this)
         implicit none
         type(var_set),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_var_set(this)
         implicit none
         type(var_set),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_var_set(this,un)
         implicit none
         type(var_set),intent(in) :: this
         integer,intent(in) :: un
         call export(this%T,un)
         call export(this%U,un)
         call export(this%p,un)
         call export(this%B,un)
         call export(this%B0,un)
         call export(this%phi,un)
         call export(this%rho,un)
       end subroutine

       subroutine import_var_set(this,un)
         implicit none
         type(var_set),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%T,un)
         call import(this%U,un)
         call import(this%p,un)
         call import(this%B,un)
         call import(this%B0,un)
         call import(this%phi,un)
         call import(this%rho,un)
       end subroutine

       subroutine display_wrap_var_set(this,dir,name)
         implicit none
         type(var_set),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_var_set(this,dir,name)
         implicit none
         type(var_set),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_var_set(this,dir,name)
         implicit none
         type(var_set),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module