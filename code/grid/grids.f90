       module grids_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
       use IO_tools_mod
       use grid_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: grids
       public :: init,delete
       public :: print,export ! import
       public :: restrict,restrict_x,restrict_xy
       public :: init_Stencils

#ifdef _DEBUG_COORDINATES_
      public :: checkgrids
#endif

       type grids
         integer :: s ! Number of grids
         type(grid),dimension(:),allocatable :: g
       end type

       interface init;           module procedure initgridsCopy;        end interface
       interface init;           module procedure initgrids1;           end interface
       interface init;           module procedure initgrids2;           end interface
       interface init;           module procedure initgrids3;           end interface
       interface delete;         module procedure deletegrids;          end interface

       interface restrict;       module procedure restrictgrids1;       end interface
       interface restrict;       module procedure restrictgrids3;       end interface
       interface restrict_x;     module procedure restrictgrids_x;      end interface
       interface restrict_xy;    module procedure restrictgrids_xy;     end interface

       interface print;          module procedure printgrids;           end interface
       interface export;         module procedure exportgrids_light;    end interface
       interface addToFile;      module procedure addToFilegrids;       end interface
       interface init_Stencils;  module procedure init_Stencils_grids;  end interface

       contains

       subroutine deletegrids(gs)
         implicit none
         type(grids),intent(inout) :: gs
         integer :: i
         if (.not.allocated(gs%g)) stop 'Error: grids not allocated in deletegrids in grids.f90'
         do i = 1,gs%s; call delete(gs%g(i)) ;enddo
       end subroutine

       subroutine initgridsCopy(gs,gs_in)
         implicit none
         type(grids),intent(inout) :: g
         type(grids),intent(in) :: gs_in
         integer :: i
         if (.not.allocated(gs%g)) stop 'Error: grids not allocated in initgridsCopy in grids.f90'
         do i = 1,gs%s; call init(gs%g(i),gs_in%g(i)) ;enddo
       end subroutine

       subroutine initgrids1(gs,g)
         implicit none
         type(grids),intent(inout) :: gs
         type(grid),intent(inout) :: g
         integer :: i
         if (.not.allocated(gs%g)) stop 'Error: grids not allocated in initgrids1 in grids.f90'
         call init(g%c(dir),h,gridsType)
       end subroutine

       ! ------------------- restrict (for multigrids) --------------

       subroutine restrictgrids1(rs,gs,dir)
         type(grids),intent(inout) :: rs
         type(grids),intent(in) :: gs
         integer,intent(in) :: dir
         if (.not.allocated(gs%g)) stop 'Error: grids not allocated in restrictgrids1 in grids.f90'
         do i = 1,gs%s; call restrict(rs%g(i)%c(dir),gs%g(i)%c(dir)) ;enddo
       end subroutine

       subroutine restrictgrids3(rs,gs)
         type(grids),intent(inout) :: rs
         type(grids),intent(in) :: gs
         integer :: i
         if (.not.allocated(rs%g)) stop 'Error: grids not allocated in restrictgrids3 in grids.f90'
         do i = 1,gs%s; call restrict(rs%g(i),gs%g(i)) ;enddo
       end subroutine

       subroutine restrictgrids_x(rs,gs)
         type(grids),intent(inout) :: rs
         type(grids),intent(in) :: gs
         integer :: i
         if (.not.allocated(rs%g)) stop 'Error: grids not allocated in restrictgrids_x in grids.f90'
         do i = 1,gs%s; call restrict(rs%g(i)%c(1),gs%g(i)%c(1)) ;enddo
       end subroutine

       subroutine restrictgrids_xy(r,g)
         type(grids),intent(inout) :: r
         type(grids),intent(in) :: g
         integer :: i
         if (.not.allocated(rs%g)) stop 'Error: grids not allocated in restrictgrids_xy in grids.f90'
         do i = 1,gs%s; call restrict(rs%g(i)%c(1),gs%g(i)%c(1)) ;enddo
         do i = 1,gs%s; call restrict(rs%g(i)%c(2),gs%g(i)%c(2)) ;enddo
       end subroutine

       ! ---------------------------------------------- check grids

#ifdef _CHECK_grids_
       subroutine checkgrids(gs)
         implicit none
         type(grids),intent(in) :: gs
         integer :: i
         do i = 1,gs%s; call checkCoordinates(gs%g(i)) ;enddo
       end subroutine
#endif

       subroutine exportgrids_light(g,dir,name)
         implicit none
         type(grids), intent(in) :: gs
         character(len=*),intent(in) :: dir,name
         integer :: i
         do i = 1,gs%s; call export(gs%g(i)) ;enddo
       end subroutine

       subroutine printgrids(g)
         implicit none
         type(grids), intent(in) :: gs
         integer :: i
         do i = 1,gs%s; call print(gs%g(i)) ;enddo
       end subroutine

       subroutine init_Stencils_grids(g)
         implicit none
         type(grids), intent(inout) :: g
         integer :: i
         do i = 1,gs%s; call init_stencils(gs%g(i)) ;enddo
       end subroutine

       end module