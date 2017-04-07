       module export_planes_mod
       use export_plane_mod
       implicit none

       private
       public :: export_planes
       public :: init,delete,export,import,display,print

       public :: add

       type export_planes
         integer :: N = 0
         type(export_plane),dimension(:),allocatable :: EP
       end type

       interface init;      module procedure init_EPS;      end interface
       interface init;      module procedure init_copy_EPS; end interface
       interface delete;    module procedure delete_EPS;    end interface
       interface export;    module procedure export_EPS;    end interface
       interface import;    module procedure import_EPS;    end interface
       interface display;   module procedure display_EPS;   end interface
       interface print;     module procedure print_EPS;     end interface

       interface add;       module procedure add_EPS;       end interface
       interface add;       module procedure add_EPS_EP;   end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_EPS(EPS,export_ever,dir,plane,suffix)
         implicit none
         type(export_planes),intent(inout) :: EPS
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,intent(in) :: plane
         character(len=1),intent(in) :: suffix
         call delete(EPS)
         allocate(EPS%EP(1))
         call init(EPS%EP(1),export_ever,dir,plane,suffix)
         EPS%N = 1
       end subroutine

       subroutine add_EPS(EPS,export_ever,dir,plane,suffix)
         implicit none
         type(export_planes),intent(inout) :: EPS
         logical,intent(in) :: export_ever
         integer,intent(in) :: dir
         integer,intent(in) :: plane
         character(len=1),intent(in) :: suffix
         type(export_planes) :: temp
         integer :: i
         if (allocated(EPS%EP)) then
           if (size(EPS%EP).gt.0) then
             call init(temp,EPS)
             call delete(EPS)
             allocate(EPS%EP(temp%N+1))
             EPS%N = temp%N+1
             do i=1,temp%N
               call init(EPS%EP(i),temp%EP(i))
             enddo
             call init(EPS%EP(EPS%N),export_ever,dir,plane,suffix)
             call delete(temp)
           else; call init(EPS,export_ever,dir,plane,suffix)
           endif
         else; call init(EPS,export_ever,dir,plane,suffix)
         endif
       end subroutine

       subroutine add_EPS_EP(EPS,EP_in)
         implicit none
         type(export_planes),intent(inout) :: EPS
         type(export_plane),intent(in) :: EP_in
         call add(EPS,EP_in%export_ever,EP_in%dir,EP_in%plane,EP_in%suffix)
       end subroutine

       subroutine init_copy_EPS(EPS,EPS_in)
         implicit none
         type(export_planes),intent(inout) :: EPS
         type(export_planes),intent(in) :: EPS_in
         integer :: i
         call delete(EPS)
         allocate(EPS%EP(EPS_in%N))
         EPS%N = EPS_in%N
         do i=1,EPS_in%N
           call init(EPS%EP(i),EPS_in%EP(i))
         enddo
       end subroutine

       subroutine delete_EPS(EPS)
         implicit none
         type(export_planes),intent(inout) :: EPS
         integer :: i
         if (allocated(EPS%EP)) then
         do i=1,size(EPS%EP); call delete(EPS%EP(i)); enddo
         deallocate(EPS%EP)
         endif
       end subroutine

       subroutine export_EPS(EPS,un)
         implicit none
         type(export_planes),intent(in) :: EPS
         integer,intent(in) :: un
         integer :: i
         write(un,*) ' -------- export_planes -------- '
         write(un,*) 'N = '; write(un,*) EPS%N
         if (allocated(EPS%EP)) then
         do i=1,size(EPS%EP); call export(EPS%EP(i),un); enddo
         endif
       end subroutine

       subroutine import_EPS(EPS,un)
         implicit none
         type(export_planes),intent(inout) :: EPS
         integer,intent(in) :: un
         integer :: i,N
         call delete(EPS)
         read(un,*)
         read(un,*); read(un,*) N
         if (N.gt.0) then
           allocate(EPS%EP(N))
           do i=1,N; call import(EPS%EP(i),un); enddo
         endif
       end subroutine

       subroutine display_EPS(EPS,un)
         implicit none
         type(export_planes),intent(in) :: EPS
         integer,intent(in) :: un
         integer :: i
         if (allocated(EPS%EP)) then
         do i=1,size(EPS%EP); call display(EPS%EP(i),un); enddo
         endif
       end subroutine

       subroutine print_EPS(EPS)
         implicit none
         type(export_planes),intent(inout) :: EPS
         call display(EPS,6)
       end subroutine

       end module