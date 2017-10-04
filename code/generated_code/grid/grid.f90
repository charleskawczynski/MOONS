       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module grid_mod
       use current_precision_mod
       use IO_tools_mod
       use coordinates_mod
       use datatype_conversion_mod
       use dir_manip_mod
       use string_mod
       implicit none

       private
       public :: grid
       public :: init,delete,display,print,export,import
       public :: display_short,print_short

       public :: export_primitives,import_primitives

       public :: export_structured,import_structured

       public :: set_IO_dir,make_IO_dir

       public :: suppress_warnings

       interface init;             module procedure init_copy_grid;          end interface
       interface delete;           module procedure delete_grid;             end interface
       interface display;          module procedure display_grid;            end interface
       interface display_short;    module procedure display_short_grid;      end interface
       interface display;          module procedure display_wrap_grid;       end interface
       interface print;            module procedure print_grid;              end interface
       interface print_short;      module procedure print_short_grid;        end interface
       interface export;           module procedure export_grid;             end interface
       interface export_primitives;module procedure export_primitives_grid;  end interface
       interface import;           module procedure import_grid;             end interface
       interface export_structured;module procedure export_structured_D_grid;end interface
       interface import_structured;module procedure import_structured_D_grid;end interface
       interface import_primitives;module procedure import_primitives_grid;  end interface
       interface export;           module procedure export_wrap_grid;        end interface
       interface import;           module procedure import_wrap_grid;        end interface
       interface set_IO_dir;       module procedure set_IO_dir_grid;         end interface
       interface make_IO_dir;      module procedure make_IO_dir_grid;        end interface
       interface suppress_warnings;module procedure suppress_warnings_grid;  end interface

       type grid
         type(coordinates),dimension(3) :: c
         real(cp) :: volume = 0.0_cp
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_grid(this,that)
         implicit none
         type(grid),intent(inout) :: this
         type(grid),intent(in) :: that
         integer :: i_c
         integer :: s_c
         call delete(this)
         s_c = size(that%c)
         do i_c=1,s_c
           call init(this%c(i_c),that%c(i_c))
         enddo
         this%volume = that%volume
         this%defined = that%defined
       end subroutine

       subroutine delete_grid(this)
         implicit none
         type(grid),intent(inout) :: this
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         do i_c=1,s_c
           call delete(this%c(i_c))
         enddo
         this%volume = 0.0_cp
         this%defined = .false.
       end subroutine

       subroutine display_grid(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         do i_c=1,s_c
           call display(this%c(i_c),un)
         enddo
         write(un,*) 'volume  = ',this%volume
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_short_grid(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         do i_c=1,s_c
           call display(this%c(i_c),un)
         enddo
         write(un,*) 'volume  = ',this%volume
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_wrap_grid(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine print_grid(this)
         implicit none
         type(grid),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_grid(this)
         implicit none
         type(grid),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_grid(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         write(un,*) s_c
         do i_c=1,s_c
           call export(this%c(i_c),un)
         enddo
         write(un,*) 'volume   = ';write(un,*) this%volume
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_grid(this,un)
         implicit none
         type(grid),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: s_c
         call delete(this)
         read(un,*) s_c
         if (s_c.gt.0) then
           do i_c=1,s_c
             call import(this%c(i_c),un)
           enddo
         endif
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine export_primitives_grid(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'volume   = ';write(un,*) this%volume
         write(un,*) 'defined  = ';write(un,*) this%defined
       end subroutine

       subroutine import_primitives_grid(this,un)
         implicit none
         type(grid),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine export_wrap_grid(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_grid(this,dir,name)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine set_IO_dir_grid(this,dir)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_c
         integer :: s_c
         call suppress_warnings(this)
         s_c = size(this%c)
         do i_c=1,s_c
           call set_IO_dir(this%c(i_c),dir//'c_'//int2str(i_c)//fortran_PS)
         enddo
       end subroutine

       subroutine make_IO_dir_grid(this,dir)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_c
         integer :: s_c
         call suppress_warnings(this)
         call make_dir_quiet(dir)
         s_c = size(this%c)
         do i_c=1,s_c
           call make_IO_dir(this%c(i_c),dir//'c_'//int2str(i_c)//fortran_PS)
         enddo
       end subroutine

       subroutine export_structured_D_grid(this,dir)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir
         integer :: i_c
         integer :: s_c
         integer :: un
         un = new_and_open(dir,'primitives')
         call export_primitives(this,un)
         s_c = size(this%c)
         write(un,*) s_c
         do i_c=1,s_c
           call export_structured(this%c(i_c),&
           dir//'c_'//int2str(i_c)//fortran_PS)
         enddo
         close(un)
       end subroutine

       subroutine import_structured_D_grid(this,dir)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir
         integer :: i_c
         integer :: s_c
         integer :: un
         un = open_to_read(dir,'primitives')
         call import_primitives(this,un)
         s_c = size(this%c)
         do i_c=1,s_c
           call import_structured(this%c(i_c),&
           dir//'c_'//int2str(i_c)//fortran_PS)
         enddo
         close(un)
       end subroutine

       subroutine suppress_warnings_grid(this)
         implicit none
         type(grid),intent(in) :: this
         if (.false.) then
           call print(this)
         endif
       end subroutine

       end module