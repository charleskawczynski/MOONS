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
       public :: display_short,print_short

       interface init;         module procedure init_copy_gr;    end interface
       interface delete;       module procedure delete_gr;       end interface
       interface display;      module procedure display_gr;      end interface
       interface display_short;module procedure display_short_gr;end interface
       interface display;      module procedure display_wrap_gr; end interface
       interface print;        module procedure print_gr;        end interface
       interface print_short;  module procedure print_short_gr;  end interface
       interface export;       module procedure export_gr;       end interface
       interface import;       module procedure import_gr;       end interface
       interface export;       module procedure export_wrap_gr;  end interface
       interface import;       module procedure import_wrap_gr;  end interface

       type grid
         type(coordinates),dimension(3) :: c
         real(cp) :: volume = 0.0_cp
         logical :: defined = .false.
       end type

       contains

       subroutine init_copy_gr(this,that)
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

       subroutine delete_gr(this)
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

       subroutine display_gr(this,un)
         implicit none
         type(grid),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_c
         integer :: s_c
         s_c = size(this%c)
         do i_c=1,s_c
           call display(this%c(i_c),un)
         enddo
       end subroutine

       subroutine display_short_gr(this,un)
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

       subroutine print_gr(this)
         implicit none
         type(grid),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_short_gr(this)
         implicit none
         type(grid),intent(in) :: this
         call display_short(this,6)
       end subroutine

       subroutine export_gr(this,un)
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

       subroutine import_gr(this,un)
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

       subroutine display_wrap_gr(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrap_gr(this,dir,name)
         implicit none
         type(grid),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrap_gr(this,dir,name)
         implicit none
         type(grid),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module