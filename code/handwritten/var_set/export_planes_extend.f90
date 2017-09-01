       module export_planes_extend_mod
       use export_planes_mod
       use export_plane_mod
       use export_plane_extend_mod
       implicit none

       private
       public :: init
       public :: add

       interface init;      module procedure init_EPS;      end interface
       interface add;       module procedure add_EPS;       end interface
       interface add;       module procedure add_EPS_EP;    end interface

       contains

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

       end module