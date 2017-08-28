       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_line_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_line
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_line;           end interface
       interface delete; module procedure delete_export_line;         end interface
       interface display;module procedure display_export_line;        end interface
       interface display;module procedure display_wrapper_export_line;end interface
       interface print;  module procedure print_export_line;          end interface
       interface export; module procedure export_export_line;         end interface
       interface import; module procedure import_export_line;         end interface
       interface export; module procedure export_wrapper_export_line; end interface
       interface import; module procedure import_wrapper_export_line; end interface

       type export_line
         logical :: export_ever = .false.
         integer :: dir = 0
         integer,dimension(2) :: line = 0
         character(len=1) :: suffix = ' '
       end type

       contains

       subroutine init_export_line(this,that)
         implicit none
         type(export_line),intent(inout) :: this
         type(export_line),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%dir = that%dir
         this%line = that%line
         this%suffix = that%suffix
       end subroutine

       subroutine delete_export_line(this)
         implicit none
         type(export_line),intent(inout) :: this
         this%export_ever = .false.
         this%dir = 0
         this%line = 0
         this%suffix = ' '
       end subroutine

       subroutine display_export_line(this,un)
         implicit none
         type(export_line),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_line'
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'line        = ',this%line
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine print_export_line(this)
         implicit none
         type(export_line),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_line(this,un)
         implicit none
         type(export_line),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
         write(un,*) 'dir          = ';write(un,*) this%dir
         write(un,*) 'line         = ';write(un,*) this%line
         write(un,*) 'suffix       = ';write(un,*) this%suffix
       end subroutine

       subroutine import_export_line(this,un)
         implicit none
         type(export_line),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%dir
         read(un,*); read(un,*) this%line
         read(un,*); read(un,*) this%suffix
       end subroutine

       subroutine display_wrapper_export_line(this,dir,name)
         implicit none
         type(export_line),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_line(this,dir,name)
         implicit none
         type(export_line),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_line(this,dir,name)
         implicit none
         type(export_line),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module