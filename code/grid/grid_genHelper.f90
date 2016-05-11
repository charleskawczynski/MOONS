       module grid_genHelper_mod
       ! This module is operates on node data only, 
       ! since it describes the physical domain.
       use current_precision_mod
       use grid_mod
       implicit none

       private

       type gridGenerator
         type(grid) :: g
       end type
       
       public :: gridGenerator,init,delete
       public :: prep,app
       public :: mirror,shift
       public :: preMirror,appMirror
       public :: prepGhost,appGhost
       public :: applyGhost
       public :: pop,snip
       public :: get_boundary_face

       interface init;        module procedure initGridGen;        end interface
       interface delete;      module procedure deleteGridGen;      end interface
       interface prep;        module procedure prepGrid;           end interface
       interface app;         module procedure appGrid;            end interface
       interface applyGhost;  module procedure applyGhost_dir;     end interface
       interface applyGhost;  module procedure applyGhost_all;     end interface
       interface mirror;      module procedure mirror_grid;        end interface
       interface shift;       module procedure shift_grid;         end interface
       interface preMirror;   module procedure prepMirrorGrid;     end interface
       interface appMirror;   module procedure appMirrorGrid;      end interface

       contains

       subroutine initGridGen(gg,h,dir)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         call delete(gg%g%c(dir))
         call init(gg%g,h,dir)
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
         call init(gg%g,(/h,temp/),dir)
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
         call init(gg%g,(/temp,h/),dir)
         deallocate(temp)
       end subroutine

       subroutine shift_grid(gg,h0,dir)
         ! shift hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),intent(in) :: h0
         real(cp),dimension(:),allocatable :: temp
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn + h0
         call init(gg%g,temp,dir)
         deallocate(temp)
       end subroutine

       subroutine mirror_grid(gg,dir)
         ! mirror hn to grid along dir
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         real(cp),dimension(:),allocatable :: temp
         integer :: s
         s = gg%g%c(dir)%sn
         if (.not.allocated(gg%g%c(dir)%hn)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn(s:1:-1)
         call init(gg%g,temp,dir)
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
         call init(gg%g,(/h1,h2/),dir)
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
         call init(gg%g,(/h1,h2/),dir)
         deallocate(h1,h2)
       end subroutine

       subroutine prepGhost(gg,dir)
         ! Prepends a ghost point
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
         call init(gg%g,temp,dir)
         deallocate(temp)
       end subroutine

       subroutine appGhost(gg,dir)
         ! Appends a ghost point
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
         call init(gg%g,temp,dir)
         deallocate(temp)
       end subroutine

       subroutine applyGhost_dir(gg,dir)
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
         call init(gg%g,temp,dir)
         deallocate(temp)
       end subroutine

       subroutine applyGhost_all(gg)
         implicit none
         type(gridGenerator),intent(inout) :: gg
         call applyGhost(gg,1); call applyGhost(gg,2); call applyGhost(gg,3)
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
         call init(gg%g,temp,dir)
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
         call init(gg%g,temp,dir)
         deallocate(temp)
       end subroutine

       subroutine get_boundary_face(gg,face)
         ! Removes all cells except for prescribed face
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: face
         integer :: i,dir,s
         select case (face)
         case (1,4); dir = 1
         case (2,5); dir = 2
         case (3,6); dir = 3
         case default; stop 'Error: face (1) must = 1:6 in get_boundary_face in grid_genHelper.f90'
         end select
         ! The number of times that snip / pop are called
         ! must be chosen carefully, this is just a test...
         s = gg%g%c(dir)%N
         select case (face)
         case (1,3,5); do i=1,s; call snip(gg,dir); enddo
         case (2,4,6); do i=1,s; call  pop(gg,dir); enddo
         case default; stop 'Error: face (2) must = 1:6 in get_boundary_face in grid_genHelper.f90'
         end select
       end subroutine

       end module