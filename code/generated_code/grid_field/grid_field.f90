       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module grid_field_mod
       use current_precision_mod
       use IO_tools_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: grid_field
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_grid_field;          end interface
       interface delete;           module procedure delete_grid_field;             end interface
       interface display;          module procedure display_grid_field;            end interface
       interface display_short;    module procedure display_short_grid_field;      end interface
       interface display;          module procedure display_wrap_grid_field;       end interface
       interface print;            module procedure print_grid_field;              end interface
       interface print_short;      module procedure print_short_grid_field;        end interface
       interface export;           module procedure export_grid_field;             end interface
       interface export_primitives;module procedure export_primitives_grid_field;  end interface
       interface import;           module procedure import_grid_field;             end interface
       interface export_structured;module procedure export_structured_D_grid_field;end interface
       interface import_structured;module procedure import_structured_D_grid_field;end interface
       interface import_primitives;module procedure import_primitives_grid_field;  end interface
       interface export;           module procedure export_wrap_grid_field;        end interface
       interface import;           module procedure import_wrap_grid_field;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_grid_field;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_grid_field;        end interface
       interface suppress_warnings;module procedure suppress_warnings_grid_field;  end interface

       type grid_field
         real(cp),dimension(:,:,:),allocatable :: f
         integer,dimension(3) :: s = 0
         integer :: s_1D = 0
       end type

       contains

       subroutine init_copy_grid_field(this,that)
         implicit none
         type(grid_field),intent(inout) :: this
         type(grid_field),intent(in) :: that
         call delete(this)
         if (allocated(that%f)) then
           this%f = that%f
         endif
         this%s = that%s
         this%s_1D = that%s_1D
       end subroutine

       subroutine delete_grid_field(this)
         implicit none
         type(grid_field),intent(inout) :: this
         if (allocated(this%f)) then
           deallocate(this%f)
         endif
         this%s = 0
         this%s_1D = 0
       end subroutine

       subroutine display_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'f    = ',this%f
         write(un,*) 's    = ',this%s
         write(un,*) 's_1D = ',this%s_1D
       end subroutine

       subroutine display_short_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 's    = ',this%s
         write(un,*) 's_1D = ',this%s_1D
       end subroutine

       subroutine display_wrap_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_grid_field(this)
         implicit none
         type(grid_field),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_grid_field(this)
         implicit none
         type(grid_field),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         if (allocated(this%f)) then
           s_f = shape(this%f)
           write(un,*) s_f
           if (all((/s_f(1).gt.0,s_f(2).gt.0,s_f(3).gt.0/))) then
             write(un,*) 'f     = ';write(un,*) this%f
           endif
         else
           write(un,*) 0
         endif
         write(un,*) 's     = ';write(un,*) this%s
         write(un,*) 's_1D  = ';write(un,*) this%s_1D
       end subroutine

       subroutine import_grid_field(this,un)
         implicit none
         type(grid_field),intent(inout) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         call delete(this)
         read(un,*) s_f
         if (all((/s_f(1).gt.0,s_f(2).gt.0,s_f(3).gt.0/))) then
           allocate(this%f(s_f(1),s_f(2),s_f(3)))
           read(un,*); read(un,*) this%f
         endif
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%s_1D
       end subroutine

       subroutine export_primitives_grid_field(this,un)
         implicit none
         type(grid_field),intent(in) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         if (allocated(this%f)) then
           s_f = shape(this%f)
           write(un,*) s_f
           if (all((/s_f(1).gt.0,s_f(2).gt.0,s_f(3).gt.0/))) then
             write(un,*) 'f     = ';write(un,*) this%f
           endif
         else
           write(un,*) 0
         endif
         write(un,*) 's     = ';write(un,*) this%s
         write(un,*) 's_1D  = ';write(un,*) this%s_1D
       end subroutine

       subroutine import_primitives_grid_field(this,un)
         implicit none
         type(grid_field),intent(inout) :: this
         integer,intent(in) :: un
         integer,dimension(3) :: s_f
         read(un,*) s_f
         if (all((/s_f(1).gt.0,s_f(2).gt.0,s_f(3).gt.0/))) then
           allocate(this%f(s_f(1),s_f(2),s_f(3)))
           read(un,*); read(un,*) this%f
         endif
         read(un,*); read(un,*) this%s
         read(un,*); read(un,*) this%s_1D
       end subroutine

       subroutine export_wrap_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_grid_field(this,dir,name)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_grid_field(this,dir)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         if (.false.) then
           write(*,*) dir
         endif
       end subroutine

       subroutine make_IO_dir_grid_field(this,dir)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir
         call suppress_warnings(this)
         call make_dir_quiet(dir)
       end subroutine

       subroutine export_structured_D_grid_field(this,dir)
         implicit none
         type(grid_field),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         close(un)
       end subroutine

       subroutine import_structured_D_grid_field(this,dir)
         implicit none
         type(grid_field),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         close(un)
       end subroutine

       subroutine suppress_warnings_grid_field(this)
         implicit none
         type(grid_field),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module