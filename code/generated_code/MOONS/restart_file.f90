       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module restart_file_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: restart_file
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_restart_file;          end interface
       interface delete;           module procedure delete_restart_file;             end interface
       interface display;          module procedure display_restart_file;            end interface
       interface display_short;    module procedure display_short_restart_file;      end interface
       interface display;          module procedure display_wrap_restart_file;       end interface
       interface print;            module procedure print_restart_file;              end interface
       interface print_short;      module procedure print_short_restart_file;        end interface
       interface export;           module procedure export_restart_file;             end interface
       interface export_primitives;module procedure export_primitives_restart_file;  end interface
       interface import;           module procedure import_restart_file;             end interface
       interface export_structured;module procedure export_structured_D_restart_file;end interface
       interface import_structured;module procedure import_structured_D_restart_file;end interface
       interface import_primitives;module procedure import_primitives_restart_file;  end interface
       interface export;           module procedure export_wrap_restart_file;        end interface
       interface import;           module procedure import_wrap_restart_file;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_restart_file;         end interface
       interface suppress_warnings;module procedure suppress_warnings_restart_file;  end interface

       type restart_file
         logical :: restart_input_file = .false.
         logical :: restart_fields = .false.
       end type

       contains

       subroutine init_copy_restart_file(this,that)
         implicit none
         type(restart_file),intent(inout) :: this
         type(restart_file),intent(in) :: that
         call delete(this)
         this%restart_input_file = that%restart_input_file
         this%restart_fields = that%restart_fields
       end subroutine

       subroutine delete_restart_file(this)
         implicit none
         type(restart_file),intent(inout) :: this
         this%restart_input_file = .false.
         this%restart_fields = .false.
       end subroutine

       subroutine display_restart_file(this,un)
         implicit none
         type(restart_file),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'restart_input_file = ',this%restart_input_file
         write(un,*) 'restart_fields     = ',this%restart_fields
       end subroutine

       subroutine display_short_restart_file(this,un)
         implicit none
         type(restart_file),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'restart_input_file = ',this%restart_input_file
         write(un,*) 'restart_fields     = ',this%restart_fields
       end subroutine

       subroutine display_wrap_restart_file(this,dir,name)
         implicit none
         type(restart_file),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_restart_file(this)
         implicit none
         type(restart_file),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_restart_file(this)
         implicit none
         type(restart_file),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_restart_file(this,un)
         implicit none
         type(restart_file),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'restart_input_file  = ';write(un,*) this%restart_input_file
         write(un,*) 'restart_fields      = ';write(un,*) this%restart_fields
       end subroutine

       subroutine import_restart_file(this,un)
         implicit none
         type(restart_file),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         read(un,*); read(un,*) this%restart_input_file
         read(un,*); read(un,*) this%restart_fields
       end subroutine

       subroutine export_primitives_restart_file(this,un)
         implicit none
         type(restart_file),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'restart_input_file  = ';write(un,*) this%restart_input_file
         write(un,*) 'restart_fields      = ';write(un,*) this%restart_fields
       end subroutine

       subroutine import_primitives_restart_file(this,un)
         implicit none
         type(restart_file),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%restart_input_file
         read(un,*); read(un,*) this%restart_fields
       end subroutine

       subroutine export_wrap_restart_file(this,dir,name)
         implicit none
         type(restart_file),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_restart_file(this,dir,name)
         implicit none
         type(restart_file),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_restart_file(this,dir)
         implicit none
         type(restart_file),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_restart_file(this,dir)
         implicit none
         type(restart_file),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_restart_file(this,dir)
         implicit none
         type(restart_file),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_restart_file(this)
         implicit none
         type(restart_file),intent(in) :: this
         if (.false.) call print(this)
       end subroutine

       end module