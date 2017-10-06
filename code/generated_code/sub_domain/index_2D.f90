       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module index_2D_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: index_2D
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_structured,import_structured,&
       import_primitives,export,import,set_IO_dir,make_IO_dir,&
       suppress_warnings

       interface init;             module procedure init_copy_index_2D;          end interface
       interface delete;           module procedure delete_index_2D;             end interface
       interface display;          module procedure display_index_2D;            end interface
       interface display_short;    module procedure display_short_index_2D;      end interface
       interface display;          module procedure display_wrap_index_2D;       end interface
       interface print;            module procedure print_index_2D;              end interface
       interface print_short;      module procedure print_short_index_2D;        end interface
       interface export;           module procedure export_index_2D;             end interface
       interface export_primitives;module procedure export_primitives_index_2D;  end interface
       interface import;           module procedure import_index_2D;             end interface
       interface export_structured;module procedure export_structured_D_index_2D;end interface
       interface import_structured;module procedure import_structured_D_index_2D;end interface
       interface import_primitives;module procedure import_primitives_index_2D;  end interface
       interface export;           module procedure export_wrap_index_2D;        end interface
       interface import;           module procedure import_wrap_index_2D;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_index_2D;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_index_2D;        end interface
       interface suppress_warnings;module procedure suppress_warnings_index_2D;  end interface

       type index_2D
         integer,dimension(2) :: i = 0
       end type

       contains

       subroutine init_copy_index_2D(this,that)
         implicit none
         type(index_2D),intent(inout) :: this
         type(index_2D),intent(in) :: that
         call delete(this)
         this%i = that%i
       end subroutine

       subroutine delete_index_2D(this)
         implicit none
         type(index_2D),intent(inout) :: this
         this%i = 0
       end subroutine

       subroutine display_index_2D(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i = ',this%i
       end subroutine

       subroutine display_short_index_2D(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i = ',this%i
       end subroutine

       subroutine display_wrap_index_2D(this,dir,name)
         implicit none
         type(index_2D),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_index_2D(this)
         implicit none
         type(index_2D),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_index_2D(this)
         implicit none
         type(index_2D),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_index_2D(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_index_2D(this,un)
         implicit none
         type(index_2D),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_index_2D(this,un)
         implicit none
         type(index_2D),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i  = ';write(un,*) this%i
       end subroutine

       subroutine import_primitives_index_2D(this,un)
         implicit none
         type(index_2D),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%i
       end subroutine

       subroutine export_wrap_index_2D(this,dir,name)
         implicit none
         type(index_2D),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_index_2D(this,dir,name)
         implicit none
         type(index_2D),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_index_2D(this,dir)
         implicit none
         type(index_2D),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_index_2D(this,dir)
         implicit none
         type(index_2D),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_index_2D(this,dir)
         implicit none
         type(index_2D),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_index_2D(this,dir)
         implicit none
         type(index_2D),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_index_2D(this)
         implicit none
         type(index_2D),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module