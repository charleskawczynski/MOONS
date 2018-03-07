       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module array_mod
       use current_precision_mod
       use datatype_conversion_mod
       use IO_tools_mod
       use string_mod
       use dir_manip_mod
       implicit none

       private
       public :: array
       public :: init,delete,display,display_short,display,print,print_short,&
       export,export_primitives,import,export_folder_structure,&
       export_structured,import_structured,import_primitives,export,import,&
       set_IO_dir,make_IO_dir,suppress_warnings

       interface init;                   module procedure init_copy_array;              end interface
       interface delete;                 module procedure delete_array;                 end interface
       interface display;                module procedure display_array;                end interface
       interface display_short;          module procedure display_short_array;          end interface
       interface display;                module procedure display_wrap_array;           end interface
       interface print;                  module procedure print_array;                  end interface
       interface print_short;            module procedure print_short_array;            end interface
       interface export;                 module procedure export_array;                 end interface
       interface export_primitives;      module procedure export_primitives_array;      end interface
       interface import;                 module procedure import_array;                 end interface
       interface export_folder_structure;module procedure export_folder_structure_array;end interface
       interface export_structured;      module procedure export_structured_D_array;    end interface
       interface import_structured;      module procedure import_structured_D_array;    end interface
       interface import_primitives;      module procedure import_primitives_array;      end interface
       interface export;                 module procedure export_wrap_array;            end interface
       interface import;                 module procedure import_wrap_array;            end interface
       interface set_IO_dir;             module procedure set_IO_dir_array;             end interface
       interface make_IO_dir;            module procedure make_IO_dir_array;            end interface
       interface suppress_warnings;      module procedure suppress_warnings_array;      end interface

       type array
         real(cp),dimension(:),allocatable :: f
         integer :: N = 0
       end type

       contains

       subroutine init_copy_array(this,that)
         implicit none
         type(array),intent(inout) :: this
         type(array),intent(in) :: that
         call delete(this)
         if (allocated(that%f)) then
           this%f = that%f
         endif
         this%N = that%N
       end subroutine

       subroutine delete_array(this)
         implicit none
         type(array),intent(inout) :: this
         if (allocated(this%f)) then
           deallocate(this%f)
         endif
         this%N = 0
       end subroutine

       subroutine display_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'f = ',this%f
         write(un,*) 'N = ',this%N
       end subroutine

       subroutine display_short_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'N = ',this%N
       end subroutine

       subroutine display_wrap_array(this,dir,name)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_array(this)
         implicit none
         type(array),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_array(this)
         implicit none
         type(array),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         call export_primitives(this,un)
       end subroutine

       subroutine import_array(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         call delete(this)
         call import_primitives(this,un)
       end subroutine

       subroutine export_primitives_array(this,un)
         implicit none
         type(array),intent(in) :: this
         integer,intent(in) :: un
         integer :: s_f
         if (allocated(this%f)) then
           s_f = size(this%f)
           write(un,*) s_f
           if (s_f.gt.0) then
             write(un,*) 'f  = ';write(un,*) this%f
           endif
         else
           write(un,*) 0
         endif
         write(un,*) 'N  = ';write(un,*) this%N
       end subroutine

       subroutine import_primitives_array(this,un)
         implicit none
         type(array),intent(inout) :: this
         integer,intent(in) :: un
         integer :: s_f
         read(un,*) s_f
         if (s_f.gt.0) then
           allocate(this%f(s_f))
           read(un,*); read(un,*) this%f
         endif
         read(un,*); read(un,*) this%N
       end subroutine

       subroutine export_wrap_array(this,dir,name)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_array(this,dir,name)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_array(this,dir)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_array(this,dir)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_folder_structure_array(this,dir)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
       end subroutine

       subroutine export_structured_D_array(this,dir)
         implicit none
         type(array),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_array(this,dir)
         implicit none
         type(array),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_array(this)
         implicit none
         type(array),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module