       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module kill_switch_mod
       use IO_tools_mod
       use string_mod
       implicit none

       private
       public :: kill_switch
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_kill_switch;           end interface
       interface delete; module procedure delete_kill_switch;         end interface
       interface display;module procedure display_kill_switch;        end interface
       interface display;module procedure display_wrapper_kill_switch;end interface
       interface print;  module procedure print_kill_switch;          end interface
       interface export; module procedure export_kill_switch;         end interface
       interface import; module procedure import_kill_switch;         end interface
       interface export; module procedure export_wrapper_kill_switch; end interface
       interface import; module procedure import_wrapper_kill_switch; end interface

       type kill_switch
         integer :: un = 0
         logical :: terminate_loop = .false.
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_kill_switch(this,that)
         implicit none
         type(kill_switch),intent(inout) :: this
         type(kill_switch),intent(in) :: that
         call delete(this)
         this%un = that%un
         this%terminate_loop = that%terminate_loop
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine delete_kill_switch(this)
         implicit none
         type(kill_switch),intent(inout) :: this
         this%un = 0
         this%terminate_loop = .false.
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine display_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- kill_switch'
         write(un,*) 'un             = ',this%un
         write(un,*) 'terminate_loop = ',this%terminate_loop
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine print_kill_switch(this)
         implicit none
         type(kill_switch),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'un              = ';write(un,*) this%un
         write(un,*) 'terminate_loop  = ';write(un,*) this%terminate_loop
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine import_kill_switch(this,un)
         implicit none
         type(kill_switch),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%un
         read(un,*); read(un,*) this%terminate_loop
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine display_wrapper_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_kill_switch(this,dir,name)
         implicit none
         type(kill_switch),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module