       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module physical_sub_domain_mod
       use IO_tools_mod
       use sub_domain_mod
       implicit none

       private
       public :: physical_sub_domain
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_physical_sub_domain;           end interface
       interface delete; module procedure delete_physical_sub_domain;         end interface
       interface display;module procedure display_physical_sub_domain;        end interface
       interface display;module procedure display_wrapper_physical_sub_domain;end interface
       interface print;  module procedure print_physical_sub_domain;          end interface
       interface export; module procedure export_physical_sub_domain;         end interface
       interface import; module procedure import_physical_sub_domain;         end interface
       interface export; module procedure export_wrapper_physical_sub_domain; end interface
       interface import; module procedure import_wrapper_physical_sub_domain; end interface

       type physical_sub_domain
         type(sub_domain) :: total
         type(sub_domain) :: physical
         logical :: defined = .false.
       end type

       contains

       subroutine init_physical_sub_domain(this,that)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         type(physical_sub_domain),intent(in) :: that
         call delete(this)
         call init(this%total,that%total)
         call init(this%physical,that%physical)
         this%defined = that%defined
       end subroutine

       subroutine delete_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         call delete(this%total)
         call delete(this%physical)
         this%defined = .false.
       end subroutine

       subroutine display_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- physical_sub_domain'
         call display(this%total,un)
         call display(this%physical,un)
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine print_physical_sub_domain(this)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%total,un)
         call export(this%physical,un)
         write(un,*) 'defined   = ';write(un,*) this%defined
       end subroutine

       subroutine import_physical_sub_domain(this,un)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import(this%total,un)
         call import(this%physical,un)
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_physical_sub_domain(this,dir,name)
         implicit none
         type(physical_sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module