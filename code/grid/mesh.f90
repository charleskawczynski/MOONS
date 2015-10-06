       module mesh_mod
      ! Pre-processor directives: (_DEBUG_MESH_)
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
       public :: mesh
       public :: init,add,delete
       public :: print,export ! import
       public :: restrict,restrict_x,restrict_xy
       public :: init_Stencils

#ifdef _DEBUG_COORDINATES_
      public :: checkmesh
#endif

       type mesh
         integer :: s ! Number of grids
         type(grid),dimension(:),allocatable :: g
       end type

       interface add;            module procedure addGrid;             end interface
       interface init;           module procedure initmeshCopy;        end interface
       interface delete;         module procedure deletemesh;          end interface

       interface restrict;       module procedure restrictmesh1;       end interface
       interface restrict;       module procedure restrictmesh3;       end interface
       interface restrict_x;     module procedure restrictmesh_x;      end interface
       interface restrict_xy;    module procedure restrictmesh_xy;     end interface

       interface print;          module procedure printmesh;           end interface
       interface export;         module procedure exportmesh_light;    end interface
       interface addToFile;      module procedure addToFilemesh;       end interface
       interface init_Stencils;  module procedure init_Stencils_mesh;  end interface

       contains

       subroutine addGrid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         type(mesh) :: temp
         integer :: i
         if (.not.allocated(m%g)) then
           call init(m%g(1),g); m%s = 1
         else
           call init(m%g(m%s+1),g); m%s = m%s + 1
         endif
       end subroutine

       subroutine deletemesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in deletemesh in mesh.f90'
         do i = 1,m%s; call delete(m%g(i)) ;enddo
         m%s = 0
       end subroutine

       subroutine initmeshCopy(m,m_in)
         implicit none
         type(mesh),intent(inout) :: g
         type(mesh),intent(in) :: m_in
         integer :: i
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in initmeshCopy in mesh.f90'
         do i = 1,m%s; call init(m%g(i),m_in%g(i)) ;enddo
       end subroutine

       ! ------------------- restrict (for multimesh) --------------

       subroutine restrictmesh1(rm,m,dir)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in restrictmesh1 in mesh.f90'
         do i = 1,m%s; call restrict(rm%g(i)%c(dir),m%g(i)%c(dir)) ;enddo
       end subroutine

       subroutine restrictmesh3(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer :: i
         if (.not.allocated(rm%g)) stop 'Error: mesh not allocated in restrictmesh3 in mesh.f90'
         do i = 1,m%s; call restrict(rm%g(i),m%g(i)) ;enddo
       end subroutine

       subroutine restrictmesh_x(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer :: i
         if (.not.allocated(rm%g)) stop 'Error: mesh not allocated in restrictmesh_x in mesh.f90'
         do i = 1,m%s; call restrict(rm%g(i)%c(1),m%g(i)%c(1)) ;enddo
       end subroutine

       subroutine restrictmesh_xy(r,g)
         type(mesh),intent(inout) :: r
         type(mesh),intent(in) :: g
         integer :: i
         if (.not.allocated(rm%g)) stop 'Error: mesh not allocated in restrictmesh_xy in mesh.f90'
         do i = 1,m%s; call restrict(rm%g(i)%c(1),m%g(i)%c(1)) ;enddo
         do i = 1,m%s; call restrict(rm%g(i)%c(2),m%g(i)%c(2)) ;enddo
       end subroutine

       ! ---------------------------------------------- check mesh

#ifdef _DEBUG_MESH_
       subroutine checkmesh(m)
         implicit none
         type(mesh),intent(in) :: m
         integer :: i
         do i = 1,m%s; call checkCoordinates(m%g(i)) ;enddo
       end subroutine
#endif

       subroutine exportmesh_light(m,dir,name)
         implicit none
         type(mesh), intent(in) :: m
         character(len=*),intent(in) :: dir,name
         integer :: i
         do i = 1,m%s; call export(m%g(i)) ;enddo
       end subroutine

       subroutine printmesh(m)
         implicit none
         type(mesh), intent(in) :: m
         integer :: i
         do i = 1,m%s; call print(m%g(i)) ;enddo
       end subroutine

       subroutine init_Stencils_mesh(m)
         implicit none
         type(mesh), intent(inout) :: m
         integer :: i
         do i = 1,m%s; call init_stencils(m%g(i)) ;enddo
       end subroutine

       end module