       module gridGen_mod
       ! This module is operates on node data only, 
       ! since it describes the physical domain.
       use grid_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type gridGenerator
         type(grid) :: g
       end type
       
       public :: gridGenerator,init,delete
       public :: prep,app
       public :: preMirror,appMirror
       public :: prepGhost,appGhost
       public :: applyGhost
       public :: pop,snip

       interface init;        module procedure initGridGen;        end interface
       interface delete;      module procedure deleteGridGen;      end interface
       interface prep;        module procedure prepGrid;           end interface
       interface app;         module procedure appGrid;            end interface
       interface preMirror;   module procedure prepMirrorGrid;     end interface
       interface appMirror;   module procedure appMirrorGrid;      end interface

       contains

       subroutine initGridGen(gg,h,dir)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         call delete(gg%g%c(dir))
         call init(gg%g,h,dir,2)
       end subroutine

       subroutine deleteGridGen(gg)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         call delete(gg%g)
       end subroutine

       subroutine prepGrid(gg,h,dir)
         ! prepend hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to prepend'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn
         call init(gg%g,(/h,temp/),dir,2)
         deallocate(temp)
       end subroutine

       subroutine appGrid(gg,h,dir)
         ! append hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to append'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn
         call init(gg%g,(/temp,h/),dir,2)
         deallocate(temp)
       end subroutine

       subroutine prepMirrorGrid(gg,dir)
         ! prepend mirror hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: h1,h2
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(h1(s-1)); allocate(h2(s))
         h2 = gg%g%c(dir)%hn; h1 = h2(s:2:-1)
         call init(gg%g,(/h1,h2/),dir,2)
         deallocate(h1,h2)
       end subroutine

       subroutine appMirrorGrid(gg,dir)
         ! append mirror of hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: h1,h2
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to append mirror'
         allocate(h1(s-1)); allocate(h2(s))
         h2 = gg%g%c(dir)%hn; h1 = h2(s-1:1:-1)
         call init(gg%g,(/h1,h2/),dir,2)
         deallocate(h1,h2)
       end subroutine

       subroutine prepGhost(gg,dir)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = gg%g%c(dir)%sn
         allocate(temp(s+1))
         do i=1,s
          temp(i+1) = gg%g%c(dir)%hn(i)
         enddo
         temp(1) = gg%g%c(dir)%hn(1) - (gg%g%c(dir)%hn(2) - gg%g%c(dir)%hn(1))
         call init(gg%g,temp,dir,2)
         deallocate(temp)
       end subroutine

       subroutine appGhost(gg,dir)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = gg%g%c(dir)%sn
         allocate(temp(s+1))
         do i=1,s
          temp(i) = gg%g%c(dir)%hn(i)
         enddo
         temp(s+1) = gg%g%c(dir)%hn(s) + (gg%g%c(dir)%hn(s) - gg%g%c(dir)%hn(s-1))
         call init(gg%g,temp,dir,2)
         deallocate(temp)
       end subroutine

       subroutine applyGhost(gg,dir)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = gg%g%c(dir)%sn
         allocate(temp(s+2))
         do i=1,s
          temp(i+1) = gg%g%c(dir)%hn(i)
         enddo
         temp(1) = gg%g%c(dir)%hn(1) - (gg%g%c(dir)%hn(2) - gg%g%c(dir)%hn(1))
         temp(s+2) = gg%g%c(dir)%hn(s) + (gg%g%c(dir)%hn(s) - gg%g%c(dir)%hn(s-1))
         call init(gg%g,temp,dir,2)
         deallocate(temp)
       end subroutine

       ! ********************************************************************
       ! ********************************************************************
       ! *********************** TRIMMING ROUTINES **************************
       ! ********************************************************************
       ! ********************************************************************

       subroutine pop(gg,dir)
         ! Removes the last index from the grid
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = gg%g%c(dir)%sn
         allocate(temp(s-1))
         do i=1,s-1
          temp(i) = gg%g%c(dir)%hn(i)
         enddo
         call init(gg%g,temp,dir,2)
         deallocate(temp)
       end subroutine

       subroutine snip(gg,dir)
         ! Removes the first index from the grid
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = gg%g%c(dir)%sn
         allocate(temp(s-1))
         do i=2,s
          temp(i-1) = gg%g%c(dir)%hn(i)
         enddo
         call init(gg%g,temp,dir,2)
         deallocate(temp)
       end subroutine

       end module