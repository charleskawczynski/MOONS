       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module boundary_mod
       use IO_tools_mod
       use BC_logicals_mod
       use single_boundary_mod
       use string_mod
       implicit none

       private
       public :: boundary
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_boundary;           end interface
       interface delete; module procedure delete_boundary;         end interface
       interface display;module procedure display_boundary;        end interface
       interface display;module procedure display_wrapper_boundary;end interface
       interface print;  module procedure print_boundary;          end interface
       interface export; module procedure export_boundary;         end interface
       interface import; module procedure import_boundary;         end interface
       interface export; module procedure export_wrapper_boundary; end interface
       interface import; module procedure import_wrapper_boundary; end interface

       type boundary
         integer :: n = 0
         type(single_boundary),dimension(:),allocatable :: sb
         type(string) :: name
         type(bc_logicals) :: bcl
       end type

       contains

       subroutine init_boundary(this,that)
         implicit none
         type(boundary),intent(inout) :: this
         type(boundary),intent(in) :: that
         integer :: i_sb
         integer :: s_sb
         call delete(this)
         this%n = that%n
         if (allocated(that%sb)) then
           s_sb = size(that%sb)
           if (s_sb.gt.0) then
             allocate(this%sb(s_sb))
             do i_sb=1,s_sb
               call init(this%sb(i_sb),that%sb(i_sb))
             enddo
           endif
         endif
         call init(this%name,that%name)
         call init(this%bcl,that%bcl)
       end subroutine

       subroutine delete_boundary(this)
         implicit none
         type(boundary),intent(inout) :: this
         integer :: i_sb
         integer :: s_sb
         this%n = 0
         if (allocated(this%sb)) then
           s_sb = size(this%sb)
           do i_sb=1,s_sb
             call delete(this%sb(i_sb))
           enddo
           deallocate(this%sb)
         endif
         call delete(this%name)
         call delete(this%bcl)
       end subroutine

       subroutine display_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- boundary'
         integer :: i_sb
         integer :: s_sb
         write(un,*) 'n    = ',this%n
         if (allocated(this%sb)) then
           s_sb = size(this%sb)
           do i_sb=1,s_sb
             call display(this%sb(i_sb),un)
           enddo
         endif
         call display(this%name,un)
         call display(this%bcl,un)
       end subroutine

       subroutine print_boundary(this)
         implicit none
         type(boundary),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_boundary(this,un)
         implicit none
         type(boundary),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_sb
         integer :: s_sb
         write(un,*) 'n     = ';write(un,*) this%n
         if (allocated(this%sb)) then
           s_sb = size(this%sb)
           write(un,*) s_sb
           do i_sb=1,s_sb
             call export(this%sb(i_sb),un)
           enddo
         endif
         call export(this%name,un)
         call export(this%bcl,un)
       end subroutine

       subroutine import_boundary(this,un)
         implicit none
         type(boundary),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_sb
         integer :: s_sb
         call delete(this)
         read(un,*); read(un,*) this%n
         if (allocated(this%sb)) then
           read(un,*) s_sb
           do i_sb=1,s_sb
             call import(this%sb(i_sb),un)
           enddo
         endif
         call import(this%name,un)
         call import(this%bcl,un)
       end subroutine

       subroutine display_wrapper_boundary(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_boundary(this,dir,name)
         implicit none
         type(boundary),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_boundary(this,dir,name)
         implicit none
         type(boundary),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module