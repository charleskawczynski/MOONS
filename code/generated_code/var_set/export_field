       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_field_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_field
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_field;           end interface
       interface delete; module procedure delete_export_field;         end interface
       interface display;module procedure display_export_field;        end interface
       interface display;module procedure display_wrapper_export_field;end interface
       interface print;  module procedure print_export_field;          end interface
       interface export; module procedure export_export_field;         end interface
       interface import; module procedure import_export_field;         end interface
       interface export; module procedure export_wrapper_export_field; end interface
       interface import; module procedure import_wrapper_export_field; end interface

       type export_field
         logical :: export_ever = .false.
       end type

       contains

       subroutine init_export_field(this,that)
         implicit none
         type(export_field),intent(inout) :: this
         type(export_field),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
       end subroutine

       subroutine delete_export_field(this)
         implicit none
         type(export_field),intent(inout) :: this
         this%export_ever = .false.
       end subroutine

       subroutine display_export_field(this,un)
         implicit none
         type(export_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_field'
         write(un,*) 'export_ever = ',this%export_ever
       end subroutine

       subroutine print_export_field(this)
         implicit none
         type(export_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_field(this,un)
         implicit none
         type(export_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
       end subroutine

       subroutine import_export_field(this,un)
         implicit none
         type(export_field),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_ever
       end subroutine

       subroutine display_wrapper_export_field(this,dir,name)
         implicit none
         type(export_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_field(this,dir,name)
         implicit none
         type(export_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_field(this,dir,name)
         implicit none
         type(export_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module