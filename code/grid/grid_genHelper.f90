       module grid_genHelper_mod
       ! This module is operates on node data only,
       ! since it describes the physical domain.
       use current_precision_mod
       use face_edge_corner_indexing_mod
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

       interface pop;         module procedure pop_gridGenerator;  end interface
       interface snip;        module procedure snip_gridGenerator; end interface

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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to prepend'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn%f
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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to append'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn%f
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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn%f + h0
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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(temp(s))
         temp = gg%g%c(dir)%hn%f(s:1:-1)
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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to prepend mirror'
         allocate(h1(s-1)); allocate(h2(s))
         h2 = gg%g%c(dir)%hn%f; h1 = h2(s:2:-1)
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
         if (.not.allocated(gg%g%c(dir)%hn%f)) stop 'Error: no existing grid when trying to append mirror'
         allocate(h1(s-1)); allocate(h2(s))
         h2 = gg%g%c(dir)%hn%f; h1 = h2(s-1:1:-1)
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
          temp(i+1) = gg%g%c(dir)%hn%f(i)
         enddo
         temp(1) = gg%g%c(dir)%hn%f(1) - (gg%g%c(dir)%hn%f(2) - gg%g%c(dir)%hn%f(1))
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
          temp(i) = gg%g%c(dir)%hn%f(i)
         enddo
         temp(s+1) = gg%g%c(dir)%hn%f(s) + (gg%g%c(dir)%hn%f(s) - gg%g%c(dir)%hn%f(s-1))
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
          temp(i+1) = gg%g%c(dir)%hn%f(i)
         enddo
         temp(1) = gg%g%c(dir)%hn%f(1) - (gg%g%c(dir)%hn%f(2) - gg%g%c(dir)%hn%f(1))
         temp(s+2) = gg%g%c(dir)%hn%f(s) + (gg%g%c(dir)%hn%f(s) - gg%g%c(dir)%hn%f(s-1))
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

       subroutine pop_gridGenerator(gg,dir)
         ! Removes the last index from the grid
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         call pop(gg%g,dir)
       end subroutine

       subroutine snip_gridGenerator(gg,dir)
         ! Removes the first index from the grid
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: dir
         call snip(gg%g,dir)
       end subroutine

       subroutine get_boundary_face(gg,face)
         ! Removes all cells except for prescribed face
         implicit none
         type(gridGenerator),intent(inout) :: gg
         integer,intent(in) :: face
         integer :: i,dir,s
         dir = dir_given_face(face)
         ! The number of times that snip / pop are called
         ! must be chosen carefully, this is just a test...
         s = gg%g%c(dir)%N
         if (min_face(face)) then; do i=1,s; call snip(gg,dir); enddo
         elseif (max_face(face)) then; do i=1,s; call  pop(gg,dir); enddo
         else; stop 'Error: face (2) must = 1:6 in get_boundary_face in grid_genHelper.f90'
         endif
       end subroutine

       end module