       ! ***************************************************
       ! ******* THIS CODE IS GENERATED. DO NOT EDIT *******
       ! ***************************************************
       module export_planes_mod
       use IO_tools_mod
       use export_plane_mod
       implicit none

       private
       public :: export_planes
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_planes;           end interface
       interface delete; module procedure delete_export_planes;         end interface
       interface display;module procedure display_export_planes;        end interface
       interface display;module procedure display_wrapper_export_planes;end interface
       interface print;  module procedure print_export_planes;          end interface
       interface export; module procedure export_export_planes;         end interface
       interface import; module procedure import_export_planes;         end interface
       interface export; module procedure export_wrapper_export_planes; end interface
       interface import; module procedure import_wrapper_export_planes; end interface

       type export_planes
         type(export_plane),dimension(:),allocatable :: ep
         integer :: n = 0
       end type

       contains

       subroutine init_export_planes(this,that)
         implicit none
         type(export_planes),intent(inout) :: this
         type(export_planes),intent(in) :: that
         integer :: i_ep
         integer :: s_ep
         call delete(this)
         if (allocated(that%ep)) then
           s_ep = size(that%ep)
           if (s_ep.gt.0) then
             allocate(this%ep(s_ep))
             do i_ep=1,s_ep
               call init(this%ep(i_ep),that%ep(i_ep))
             enddo
           endif
         endif
         this%n = that%n
       end subroutine

       subroutine delete_export_planes(this)
         implicit none
         type(export_planes),intent(inout) :: this
         integer :: i_ep
         integer :: s_ep
         if (allocated(this%ep)) then
           s_ep = size(this%ep)
           do i_ep=1,s_ep
             call delete(this%ep(i_ep))
           enddo
           deallocate(this%ep)
         endif
         this%n = 0
       end subroutine

       subroutine display_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) ' -------------------- export_planes'
         integer :: i_ep
         integer :: s_ep
         if (allocated(this%ep)) then
           s_ep = size(this%ep)
           do i_ep=1,s_ep
             call display(this%ep(i_ep),un)
           enddo
         endif
         write(un,*) 'n  = ',this%n
       end subroutine

       subroutine print_export_planes(this)
         implicit none
         type(export_planes),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine export_export_planes(this,un)
         implicit none
         type(export_planes),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_ep
         integer :: s_ep
         if (allocated(this%ep)) then
           s_ep = size(this%ep)
           write(un,*) s_ep
           do i_ep=1,s_ep
             call export(this%ep(i_ep),un)
           enddo
         endif
         write(un,*) 'n   = ';write(un,*) this%n
       end subroutine

       subroutine import_export_planes(this,un)
         implicit none
         type(export_planes),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_ep
         integer :: s_ep
         call delete(this)
         if (allocated(this%ep)) then
           read(un,*) s_ep
           do i_ep=1,s_ep
             call import(this%ep(i_ep),un)
           enddo
         endif
         read(un,*); read(un,*) this%n
       end subroutine

       subroutine display_wrapper_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call display(this,un)
         close(un)
       end subroutine

       subroutine export_wrapper_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_export_planes(this,dir,name)
         implicit none
         type(export_planes),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module