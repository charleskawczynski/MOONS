       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_field_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_field
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       interface init;         module procedure init_copy_ex;    end interface
       interface delete;       module procedure delete_ex;       end interface
       interface display;      module procedure display_ex;      end interface
       interface display_short;module procedure display_short_ex;end interface
       interface display;      module procedure display_wrap_ex; end interface
       interface print;        module procedure print_ex;        end interface
       interface print_short;  module procedure print_short_ex;  end interface
       interface export;       module procedure export_ex;       end interface
       interface import;       module procedure import_ex;       end interface
       interface export;       module procedure export_wrap_ex;  end interface
       interface import;       module procedure import_wrap_ex;  end interface

       type export_field
         logical :: export_ever = .false.
       end type

       contains

       subroutine init_copy_ex(this,that)
         implicit none
         type(export_field),intent(inout) :: this
         type(export_field),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
       end subroutine

       subroutine delete_ex(this)
         implicit none
         type(export_field),intent(inout) :: this
         this%export_ever = .false.
       end subroutine

       subroutine display_ex(this,un)
         implicit none
         type(export_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_field'
         write(un,*) 'export_ever = ',this%export_ever
       end subroutine

       subroutine display_short_ex(this,un)
         implicit none
         type(export_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',this%export_ever
       end subroutine

       subroutine print_ex(this)
         implicit none
         type(export_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_ex(this)
         implicit none
         type(export_field),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_ex(this,un)
         implicit none
         type(export_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
       end subroutine

       subroutine import_ex(this,un)
         implicit none
         type(export_field),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_ever
       end subroutine

       subroutine display_wrap_ex(this,dir,name)
         implicit none
         type(export_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_ex(this,dir,name)
         implicit none
         type(export_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_ex(this,dir,name)
         implicit none
         type(export_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module