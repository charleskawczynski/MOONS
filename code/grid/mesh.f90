       module mesh_mod
      ! Pre-processor directives: (_DEBUG_MESH_)
       use IO_tools_mod
       use grid_mod
       use coordinates_mod
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
       public :: patch
       public :: restrict,restrict_x,restrict_xy
       public :: init_Stencils,initProps

#ifdef _DEBUG_COORDINATES_
      public :: checkmesh
#endif

       type mesh
         integer :: s       ! Number of grids
         integer,dimension(3) :: N_cells ! Number of cells, for export
         type(grid),dimension(:),allocatable :: g
         real(cp) :: volume
         real(cp),dimension(3) :: hmax,hmin
         real(cp),dimension(3) :: dhmax,dhmin
       end type

       interface init;           module procedure init_grid;           end interface
       interface add;            module procedure addGrid;             end interface
       interface init;           module procedure initmeshCopy;        end interface
       interface delete;         module procedure deletemesh;          end interface
       interface initProps;      module procedure initProps_mesh;      end interface

       interface patch;          module procedure patch_grids;         end interface

       interface restrict;       module procedure restrictmesh1;       end interface
       interface restrict;       module procedure restrictmesh3;       end interface
       interface restrict_x;     module procedure restrictmesh_x;      end interface
       interface restrict_xy;    module procedure restrictmesh_xy;     end interface

       interface print;          module procedure printmesh;           end interface
       interface export;         module procedure exportmesh_light;    end interface
       interface init_Stencils;  module procedure init_Stencils_mesh;  end interface

       contains

       subroutine init_grid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         call delete(m)
         allocate(m%g(1))
         call init(m%g(1),g); m%s = 1
         call initProps(m)
       end subroutine

       subroutine addGrid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         type(mesh) :: temp
         integer :: i
         if (allocated(m%g)) then
           call init(temp,m)
           call delete(m)
           m%s = temp%s + 1
           allocate(m%g(m%s))
           do i=1,temp%s; call init(m%g(i),temp%g(i)); enddo
           call init(m%g(m%s),g)
           call delete(temp)
         else
           allocate(m%g(1))
           call init(m%g(1),g); m%s = 1
         endif
         call initProps(m)
       end subroutine

       subroutine deletemesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         if (allocated(m%g)) then
           do i = 1,m%s; call delete(m%g(i)) ;enddo
           m%s = 0; deallocate(m%g)
         else; m%s = 0
         endif
       end subroutine

       subroutine initmeshCopy(m_out,m_in)
         implicit none
         type(mesh),intent(inout) :: m_out
         type(mesh),intent(in) :: m_in
         integer :: i
         if (.not.allocated(m_in%g)) stop 'Error: mesh not allocated in initmeshCopy in mesh.f90'
         call delete(m_out)
         allocate(m_out%g(m_in%s))
         do i = 1,m_in%s; call init(m_out%g(i),m_in%g(i)) ;enddo
         call initProps(m_out)
         m_out%s = m_in%s
       end subroutine

       subroutine initProps_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in initProps_mesh in mesh.f90'
         if (m%s.gt.1) then
           do j=1,3
             m%hmin(j)  = minval( (/(m%g(i)%c(j)%hmin  , i=2,m%s)/) )
             m%dhmin(j) = minval( (/(m%g(i)%c(j)%dhmin , i=2,m%s)/) )
             m%hmax(j)  = maxval( (/(m%g(i)%c(j)%hmax  , i=2,m%s)/) )
             m%dhmax(j) = maxval( (/(m%g(i)%c(j)%dhmax , i=2,m%s)/) )
           enddo
         else
           do j=1,3
             m%hmin(j)  = m%g(1)%c(j)%hmin
             m%dhmin(j) = m%g(1)%c(j)%dhmin
             m%hmax(j)  = m%g(1)%c(j)%hmax
             m%dhmax(j) = m%g(1)%c(j)%dhmax
           enddo
         endif
         m%N_cells = 0
         do i=1,m%s
           m%volume = m%volume + m%g(i)%volume
           do j=1,3
             m%N_cells(j) = m%N_cells(j) + m%g(i)%c(j)%N
           enddo
         enddo
       end subroutine

       subroutine patch_grids(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j,k
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in initProps_mesh in mesh.f90'
         do k=1,3 ! Remove all patches first
           do i=1,m%s; call init(m%g(i)%st(k),.false.,.false.); enddo
         enddo
         if (m%s.gt.1) then
           do k=1,3; do i=1,m%s; do j=1,m%s
             if (abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmax).lt.0.01_cp*m%dhmin(k)) then
                   m%g(i)%st(k)%hmin = .true.;  m%g(j)%st(k)%hmax = .true.
             endif
           enddo; enddo; enddo

           do k=1,3; do i=1,m%s
              call stitch_stencils(m%g(i)%c(k),m%g(i)%st(k)%hmin,m%g(i)%st(k)%hmax)
           enddo; enddo
         endif
       end subroutine

       ! ------------------- restrict (for multimesh) --------------

       subroutine restrictmesh1(rm,m,dir)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: i
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

       subroutine restrictmesh_xy(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
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
         do i = 1,m%s; call export(m%g(i),dir,name) ;enddo
       end subroutine

       subroutine printmesh(m)
         implicit none
         type(mesh), intent(in) :: m
         integer :: i
         write(*,*) ' ************* mesh ************* '
         do i = 1,m%s
           call print(m%g(i))
           write(*,*) ' -------------------------------- '
         enddo
         write(*,*) 's = ',m%s
         write(*,*) 'N_cells = ',m%N_cells
         write(*,*) 'volume = ',m%volume
         write(*,*) 'hmin = ',m%hmin
         write(*,*) 'hmax = ',m%hmax
         write(*,*) 'dhmin = ',m%dhmin
         write(*,*) 'dhmax = ',m%dhmax
         write(*,*) ' ******************************** '
       end subroutine

       subroutine init_Stencils_mesh(m)
         implicit none
         type(mesh), intent(inout) :: m
         integer :: i
         do i = 1,m%s; call init_stencils(m%g(i)) ;enddo
       end subroutine

       end module