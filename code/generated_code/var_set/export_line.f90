       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_line_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: export_line
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_export_line;          end interface
       interface delete;           module procedure delete_export_line;             end interface
       interface display;          module procedure display_export_line;            end interface
       interface display_short;    module procedure display_short_export_line;      end interface
       interface display;          module procedure display_wrap_export_line;       end interface
       interface print;            module procedure print_export_line;              end interface
       interface print_short;      module procedure print_short_export_line;        end interface
       interface export;           module procedure export_export_line;             end interface
       interface export_primitives;module procedure export_primitives_export_line;  end interface
       interface import;           module procedure import_export_line;             end interface
       interface export_structured;module procedure export_structured_D_export_line;end interface
       interface import_structured;module procedure import_structured_D_export_line;end interface
       interface import_primitives;module procedure import_primitives_export_line;  end interface
       interface export;           module procedure export_wrap_export_line;        end interface
       interface import;           module procedure import_wrap_export_line;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_export_line;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_export_line;        end interface
       interface suppress_warnings;module procedure suppress_warnings_export_line;  end interface

       type export_line
         logical :: export_ever = .false.
         integer :: dir = 0
         integer,dimension(2) :: line = 0
         character(len=1) :: suffix = ' '
       end type

       contains

       subroutine init_copy_export_line(this,that)
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
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'line        = ',this%line
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine display_short_export_line(this,un)
         implicit none
         type(export_line),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'line        = ',this%line
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine display_wrap_export_line(this,dir,name)
         implicit none
         type(export_line),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_export_line(this)
         implicit none
         type(export_line),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_export_line(this)
         implicit none
         type(export_line),intent(in) :: this
         call display_short(this,6)
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

       subroutine export_primitives_export_line(this,un)
         implicit none
         type(export_line),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever  = ';write(un,*) this%export_ever
         write(un,*) 'dir          = ';write(un,*) this%dir
         write(un,*) 'line         = ';write(un,*) this%line
         write(un,*) 'suffix       = ';write(un,*) this%suffix
       end subroutine

       subroutine import_primitives_export_line(this,un)
         implicit none
         type(export_line),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%export_ever
         read(un,*); read(un,*) this%dir
         read(un,*); read(un,*) this%line
         read(un,*); read(un,*) this%suffix
       end subroutine

       subroutine export_wrap_export_line(this,dir,name)
         implicit none
         type(export_line),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_export_line(this,dir,name)
         implicit none
         type(export_line),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_export_line(this,dir)
         implicit none
         type(export_line),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) write(*,*) dir
       end subroutine

       subroutine make_IO_dir_export_line(this,dir)
         implicit none
         type(export_line),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_export_line(this,dir)
         implicit none
         type(export_line),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Exporting export_line structured'
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_export_line(this,dir)
         implicit none
         type(export_line),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         write(*,*) 'Importing export_line structured'
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_export_line(this)
         implicit none
         type(export_line),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module