       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_now_mod
       use IO_tools_mod
       use step_mod
       use string_mod
       implicit none

       private
       public :: export_now
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_now;           end interface
       interface delete; module procedure delete_export_now;         end interface
       interface display;module procedure display_export_now;        end interface
       interface display;module procedure display_wrapper_export_now;end interface
       interface print;  module procedure print_export_now;          end interface
       interface export; module procedure export_export_now;         end interface
       interface import; module procedure import_export_now;         end interface
       interface export; module procedure export_wrapper_export_now; end interface
       interface import; module procedure import_wrapper_export_now; end interface

       type export_now
         type(step) :: u
         type(step) :: b
         type(step) :: t
         type(step) :: rho
         type(step) :: all
         logical :: any_next = .false.
         logical :: any_now = .false.
         integer :: un = 0
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_export_now(this,that)
         implicit none
         type(export_now),intent(inout) :: this
         type(export_now),intent(in) :: that
         call delete(this)
         call init(this%u,that%u)
         call init(this%b,that%b)
         call init(this%t,that%t)
         call init(this%rho,that%rho)
         call init(this%all,that%all)
         this%any_next = that%any_next
         this%any_now = that%any_now
         this%un = that%un
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_export_now(this)
         implicit none
         type(export_now),intent(inout) :: this
         call delete(this%u)
         call delete(this%b)
         call delete(this%t)
         call delete(this%rho)
         call delete(this%all)
         this%any_next = .false.
         this%any_now = .false.
         this%un = 0
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_now'
         call display(this%u,un)
         call display(this%b,un)
         call display(this%t,un)
         call display(this%rho,un)
         call display(this%all,un)
         write(un,*) 'any_next = ',this%any_next
         write(un,*) 'any_now  = ',this%any_now
         write(un,*) 'un       = ',this%un
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_export_now(this)
         implicit none
         type(export_now),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_now(this,un)
         implicit none
         type(export_now),intent(in) :: this
         integer,intent(in) :: un
         call export(this%u,un)
         call export(this%b,un)
         call export(this%t,un)
         call export(this%rho,un)
         call export(this%all,un)
         write(un,*) 'any_next  = ';write(un,*) this%any_next
         write(un,*) 'any_now   = ';write(un,*) this%any_now
         write(un,*) 'un        = ';write(un,*) this%un
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_export_now(this,un)
         implicit none
         type(export_now),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%u,un)
         call import(this%b,un)
         call import(this%t,un)
         call import(this%rho,un)
         call import(this%all,un)
         read(un,*); read(un,*) this%any_next
         read(un,*); read(un,*) this%any_now
         read(un,*); read(un,*) this%un
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrapper_export_now(this,dir,name)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_now(this,dir,name)
         implicit none
         type(export_now),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_now(this,dir,name)
         implicit none
         type(export_now),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module