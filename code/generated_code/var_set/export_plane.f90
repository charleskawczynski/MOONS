       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_plane_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: export_plane
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_restart,import_restart

       public :: make_restart_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_plane;        end interface
       interface delete;           module procedure delete_export_plane;           end interface
       interface display;          module procedure display_export_plane;          end interface
       interface display_short;    module procedure display_short_export_plane;    end interface
       interface display;          module procedure display_wrap_export_plane;     end interface
       interface print;            module procedure print_export_plane;            end interface
       interface print_short;      module procedure print_short_export_plane;      end interface
       interface export;           module procedure export_export_plane;           end interface
       interface export_primitives;module procedure export_primitives_export_plane;end interface
       interface export_restart;   module procedure export_restart_export_plane;   end interface
       interface import;           module procedure import_export_plane;           end interface
       interface import_restart;   module procedure import_restart_export_plane;   end interface
       interface import_primitives;module procedure import_primitives_export_plane;end interface
       interface export;           module procedure export_wrap_export_plane;      end interface
       interface import;           module procedure import_wrap_export_plane;      end interface
       interface make_restart_dir; module procedure make_restart_dir_export_plane; end interface
       interface suppress_warnings;module procedure suppress_warnings_export_plane;end interface

       type export_plane
         logical :: export_ever = .false.
         integer :: dir = 0
         integer :: plane = 0
         character(len=1) :: suffix = ' '
       end type

       contains

       subroutine init_copy_export_plane(this,that)
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
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine display_short_export_plane(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine display_wrap_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_plane(this)
         implicit none
         type(export_plane),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_plane(this)
         implicit none
         type(export_plane),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_primitives_export_plane(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
         write(un,*) 'dir          = ';write(un,*) this%dir
         write(un,*) 'plane        = ';write(un,*) this%plane
         write(un,*) 'suffix       = ';write(un,*) this%suffix
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

       subroutine import_primitives_export_plane(this,un)
         implicit none
         type(export_plane),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%dir
         read(un,*); read(un,*) this%plane
         read(un,*); read(un,*) this%suffix
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

       subroutine export_wrap_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_plane(this,dir,name)
         implicit none
         type(export_plane),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       subroutine make_restart_dir_export_plane(this,dir)
         implicit none
         type(export_plane),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_restart_export_plane(this,dir)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_restart_export_plane(this,dir)
         implicit none
         type(export_plane),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_export_plane(this)
         implicit none
         type(export_plane),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module