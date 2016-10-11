       module OBJ_CLASS_mod
       use IO_tools_mod

       implicit none
       private
       public :: OBJ_CLASS
       public :: init,delete,display,print,export,import ! essentials

       type OBJ_CLASS
       end type

       interface init;           module procedure init_OBJ;           end interface
       interface init;           module procedure init_copy_OBJ;      end interface
       interface delete;         module procedure delete_OBJ;         end interface
       interface display;        module procedure display_OBJ;        end interface
       interface print;          module procedure print_OBJ;          end interface
       interface export;         module procedure export_OBJ;         end interface
       interface import;         module procedure import_OBJ;         end interface
       interface export;         module procedure export_OBJ_wrapper; end interface
       interface import;         module procedure import_OBJ_wrapper; end interface

       contains

       ! ****************************************************************
       ! ************************** ESSENTIALS **************************
       ! ****************************************************************

       subroutine init_OBJ(OBJ)
         implicit none
         type(OBJ_CLASS),intent(inout) :: OBJ
       end subroutine

       subroutine init_copy_OBJ(OBJ,OBJ_in)
         implicit none
         type(OBJ_CLASS),intent(inout) :: OBJ
         type(OBJ_CLASS),intent(in) :: OBJ_in
       end subroutine

       subroutine delete_OBJ(OBJ)
         implicit none
         type(OBJ_CLASS),intent(inout) :: OBJ
       end subroutine

       subroutine display_OBJ(OBJ,un)
         implicit none
         type(OBJ_CLASS),intent(in) :: OBJ
         integer,intent(in) :: un
         write(un,*) 
       end subroutine

       subroutine print_OBJ(OBJ)
         implicit none
         type(OBJ_CLASS),intent(in) :: OBJ
         call display(OBJ,6)
       end subroutine

       subroutine export_OBJ(OBJ,un)
         implicit none
         type(OBJ_CLASS),intent(in) :: OBJ
         integer,intent(in) :: un
         write(un,*) 
       end subroutine

       subroutine import_OBJ(OBJ,un)
         implicit none
         type(OBJ_CLASS),intent(inout) :: OBJ
         integer,intent(in) :: un
         read(un,*) 
       end subroutine

       subroutine export_OBJ_wrapper(OBJ,dir,name)
         implicit none
         type(OBJ_CLASS),intent(in) :: OBJ
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(OBJ,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_OBJ_wrapper(OBJ,dir,name)
         implicit none
         type(OBJ_CLASS),intent(inout) :: OBJ
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(OBJ,un)
         call close_and_message(un,dir,name)
       end subroutine

       ! ****************************************************************
       ! **************************** OTHER *****************************
       ! ****************************************************************

       end module