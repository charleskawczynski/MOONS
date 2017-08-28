       module kill_switch_mod
       use string_mod
       use IO_tools_mod
       implicit none
       private
       public :: kill_switch
       public :: init,delete,export,import

       interface init;               module procedure init_KS;             end interface
       interface delete;             module procedure delete_KS;           end interface
       interface export;             module procedure export_KS;           end interface
       interface import;             module procedure import_KS;           end interface

       type kill_switch
         logical :: terminate_loop = .false.
         integer :: un
         type(string) :: dir,name
       end type

       contains

       subroutine init_KS(KS,dir,name)
         implicit none
         type(kill_switch),intent(inout) :: KS
         character(len=*),intent(in) :: dir,name
         KS%terminate_loop = .false.
         call init(KS%dir,dir)
         call init(KS%name,name)
         call export(KS)
       end subroutine

       subroutine delete_KS(KS)
         implicit none
         type(kill_switch),intent(inout) :: KS
         KS%terminate_loop = .false.
         call delete(KS%dir)
         call delete(KS%name)
       end subroutine

       subroutine export_KS(KS)
         implicit none
         type(kill_switch),intent(in) :: KS
         integer :: un
         un = new_and_open(str(KS%dir),str(KS%name))
         write(un,*) 'terminate_loop = ';         write(un,*) KS%terminate_loop
         call close_and_message(un,str(KS%dir),str(KS%name))
       end subroutine

       subroutine import_KS(KS)
         implicit none
         type(kill_switch),intent(inout) :: KS
         integer :: un
         un = open_to_read(str(KS%dir),str(KS%name))
         read(un,*) ;         read(un,*) KS%terminate_loop
         call close_and_message(un,str(KS%dir),str(KS%name))
       end subroutine

       end module