       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_plane_mod
       use IO_tools_mod
       implicit none

       private
       public :: export_plane
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_plane;           end interface
       interface delete; module procedure delete_export_plane;         end interface
       interface display;module procedure display_export_plane;        end interface
       interface display;module procedure display_wrapper_export_plane;end interface
       interface print;  module procedure print_export_plane;          end interface
       interface export; module procedure export_export_plane;         end interface
       interface import; module procedure import_export_plane;         end interface
       interface export; module procedure export_wrapper_export_plane; end interface
       interface import; module procedure import_wrapper_export_plane; end interface

       type export_plane
         logical :: export_ever = .false.
         integer :: dir = 0
         integer,dimension(2) :: plane = 0
         character(len=1) :: suffix = ' '
       end type

       contains

       subroutine init_export_plane(this,that)
         implicit none
         type(export_plane),intent(inout) :: this
         type(export_plane),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%dir = that%dir
         this%plane = that%plane
         this%suffix = that%suffix
       end subroutine

       subroutine delete_export_plane(this)
         implicit none
         type(export_plane),intent(inout) :: this
         this%export_ever = .false.
         this%dir = 0
         this%plane = 0
         this%suffix = ' '
       end subroutine

       subroutine display_export_plane(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_plane'
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine print_export_plane(this)
         implicit none
         type(export_plane),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_plane(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
         write(un,*) 'dir          = ';write(un,*) this%dir
         write(un,*) 'plane        = ';write(un,*) this%plane
         write(un,*) 'suffix       = ';write(un,*) this%suffix
       end subroutine

       subroutine import_export_plane(this,un)
         implicit none
         type(export_plane),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%dir
         read(un,*); read(un,*) this%plane
         read(un,*); read(un,*) this%suffix
       end subroutine

       subroutine display_wrapper_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module