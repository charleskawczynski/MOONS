       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module rk_params_mod
       use IO_tools_mod
       use array_mod
       implicit none

       private
       public :: rk_params
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_rk_params;           end interface
       interface delete; module procedure delete_rk_params;         end interface
       interface display;module procedure display_rk_params;        end interface
       interface display;module procedure display_wrapper_rk_params;end interface
       interface print;  module procedure print_rk_params;          end interface
       interface export; module procedure export_rk_params;         end interface
       interface import; module procedure import_rk_params;         end interface
       interface export; module procedure export_wrapper_rk_params; end interface
       interface import; module procedure import_wrapper_rk_params; end interface

       type rk_params
         integer :: n_stages = 0
         integer :: n = 0
         logical :: rk_active = .false.
         type(array) :: gamma
         type(array) :: zeta
         type(array) :: alpha
         type(array) :: beta
       end type

       contains

       subroutine init_rk_params(this,that)
         implicit none
         type(rk_params),intent(inout) :: this
         type(rk_params),intent(in) :: that
         call delete(this)
         this%n_stages = that%n_stages
         this%n = that%n
         this%rk_active = that%rk_active
         call init(this%gamma,that%gamma)
         call init(this%zeta,that%zeta)
         call init(this%alpha,that%alpha)
         call init(this%beta,that%beta)
       end subroutine

       subroutine delete_rk_params(this)
         implicit none
         type(rk_params),intent(inout) :: this
         this%n_stages = 0
         this%n = 0
         this%rk_active = .false.
         call delete(this%gamma)
         call delete(this%zeta)
         call delete(this%alpha)
         call delete(this%beta)
       end subroutine

       subroutine display_rk_params(this,un)
         implicit none
         type(rk_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- rk_params'
         write(un,*) 'n_stages  = ',this%n_stages
         write(un,*) 'n         = ',this%n
         write(un,*) 'rk_active = ',this%rk_active
         call display(this%gamma,un)
         call display(this%zeta,un)
         call display(this%alpha,un)
         call display(this%beta,un)
       end subroutine

       subroutine print_rk_params(this)
         implicit none
         type(rk_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_rk_params(this,un)
         implicit none
         type(rk_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'n_stages   = ';write(un,*) this%n_stages
         write(un,*) 'n          = ';write(un,*) this%n
         write(un,*) 'rk_active  = ';write(un,*) this%rk_active
         call export(this%gamma,un)
         call export(this%zeta,un)
         call export(this%alpha,un)
         call export(this%beta,un)
       end subroutine

       subroutine import_rk_params(this,un)
         implicit none
         type(rk_params),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%n_stages
         read(un,*); read(un,*) this%n
         read(un,*); read(un,*) this%rk_active
         call import(this%gamma,un)
         call import(this%zeta,un)
         call import(this%alpha,un)
         call import(this%beta,un)
       end subroutine

       subroutine display_wrapper_rk_params(this,dir,name)
         implicit none
         type(rk_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_rk_params(this,dir,name)
         implicit none
         type(rk_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_rk_params(this,dir,name)
         implicit none
         type(rk_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module