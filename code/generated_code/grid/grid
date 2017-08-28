       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module grid_mod
       use current_precision_mod
       use IO_tools_mod
       use coordinates_mod
       implicit none

       private
       public :: grid
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_grid;           end interface
       interface delete; module procedure delete_grid;         end interface
       interface display;module procedure display_grid;        end interface
       interface display;module procedure display_wrapper_grid;end interface
       interface print;  module procedure print_grid;          end interface
       interface export; module procedure export_grid;         end interface
       interface import; module procedure import_grid;         end interface
       interface export; module procedure export_wrapper_grid; end interface
       interface import; module procedure import_wrapper_grid; end interface

       type grid
         type(coordinates),dimension(3) :: c
         real(cp) :: volume = 0.0_cp
         logical :: defined = .false.
       end type

       contains

       subroutine init_grid(this,that)
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
         write(un,*) ' -------------------- grid'
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         do i_c=1,s_c
           call display(this%c(i_c),un)
         enddo
         write(un,*) 'volume  = ',this%volume
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine print_grid(this)
         implicit none
         type(grid),intent(in) :: this
         call display(this,6)
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
         do i_c=1,s_c
           call import(this%c(i_c),un)
         enddo
         read(un,*); read(un,*) this%volume
         read(un,*); read(un,*) this%defined
       end subroutine

       subroutine display_wrapper_grid(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_grid(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_grid(this,dir,name)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module