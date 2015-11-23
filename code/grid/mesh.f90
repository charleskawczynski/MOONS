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
       public :: initProps

#ifdef _DEBUG_COORDINATES_
      public :: checkmesh
#endif

       type mesh
         type(grid),dimension(:),allocatable :: g
         ! Properties
         integer :: s       ! Number of grids
         integer,dimension(3) :: N_cells ! Number of cells, for export
         integer :: N_cells_tot ! Total number of cells
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

       interface print;          module procedure print_mesh;          end interface
       interface export;         module procedure export_mesh;         end interface
       interface export;         module procedure exportmesh_all;      end interface


       contains

       subroutine init_grid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         integer :: i
         call delete(m)
         allocate(m%g(1))
         call init(m%g(1),g); m%s = 1
         call initProps(m)
         do i=1,m%s
           call delete(m%g(i)%st_face)
           call delete(m%g(i)%st_edge)
           call delete(m%g(i)%st_corner)
         enddo
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
         m_out%s = m_in%s
         allocate(m_out%g(m_in%s))
         do i = 1,m_in%s; call init(m_out%g(i),m_in%g(i)) ;enddo
         call initProps(m_out)
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
         m%N_cells = 0; m%N_cells_tot = 0; m%volume = 0.0_cp
         do i=1,m%s
           m%volume = m%volume + m%g(i)%volume
           do j=1,3
             m%N_cells(j) = m%N_cells(j) + m%g(i)%c(j)%N
           enddo
           m%N_cells_tot = m%N_cells_tot + m%g(i)%c(1)%N*m%g(i)%c(2)%N*m%g(i)%c(3)%N
         enddo
       end subroutine

       subroutine patch_grids(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         real(cp) :: tol
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in patch_grids in mesh.f90'
         if (size(m%g).ne.m%s) stop 'Error: mesh size not correct in patch_grids in mesh.f90'
         if (m%s.gt.1) then
           tol = 0.01_cp
           ! Remove all patches first
           do i=1,m%s; call delete(m%g(i)%st_face); enddo
           do i=1,m%s; call delete(m%g(i)%st_edge); enddo
           do i=1,m%s; call delete(m%g(i)%st_corner); enddo
           call initProps(m) ! Need dhmin etc.
           call patch_Faces(m,tol)
           call patch_Edges(m,tol)
           call patch_Corners(m,tol)
         endif
       end subroutine

       subroutine patch_Faces(m,tol) ! 6 faces per grid
         ! Find grids who share a face.
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         integer :: i,j,k,a1,a2
         logical,dimension(5) :: TF_face
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_face(1) = abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmax).lt.tol*m%dhmin(k) ! Contact face
             select case (k)
             case (1); a1 = 2; a2 = 3 ! orthogonal directions to contact face
             case (2); a1 = 1; a2 = 3 ! orthogonal directions to contact face
             case (3); a1 = 1; a2 = 2 ! orthogonal directions to contact face
             end select
             TF_face(2) = abs(m%g(i)%c(a1)%hmin-m%g(j)%c(a1)%hmin).lt.tol*m%dhmin(a1) ! Adjacent face 1 hmin
             TF_face(3) = abs(m%g(i)%c(a1)%hmax-m%g(j)%c(a1)%hmax).lt.tol*m%dhmin(a1) ! Adjacent face 1 hmax
             TF_face(4) = abs(m%g(i)%c(a2)%hmin-m%g(j)%c(a2)%hmin).lt.tol*m%dhmin(a2) ! Adjacent face 2 hmin
             TF_face(5) = abs(m%g(i)%c(a2)%hmax-m%g(j)%c(a2)%hmax).lt.tol*m%dhmin(a2) ! Adjacent face 2 hmax

             if (all(TF_face)) then
               m%g(i)%st_face%hmin(k) = .true.
               m%g(i)%st_face%hmin_id(k) = j
               m%g(j)%st_face%hmax(k) = .true.
               m%g(j)%st_face%hmax_id(k) = i
             endif
           endif
         enddo; enddo; enddo
         do k=1,3; do i=1,m%s
            call stitch_stencils(m%g(i)%c(k),m%g(i)%st_face%hmin(k),m%g(i)%st_face%hmax(k))
         enddo; enddo
       end subroutine

       subroutine patch_Edges(m,tol) ! 12 edges per grid
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         call patch_Edges_diag(m,tol)
         call patch_Edges_adjoin_face(m)
       end subroutine

       subroutine patch_Edges_diag(m,tol) ! 12 edges per grid
         ! Find grids who share an edge, but NOT a face.
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         integer :: i,j,k,a1,a2
         logical,dimension(6) :: TF_edge
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_edge(1) = abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmin).lt.tol*m%dhmin(k) ! Edge direction (min)
             TF_edge(2) = abs(m%g(i)%c(k)%hmax-m%g(j)%c(k)%hmax).lt.tol*m%dhmin(k) ! Edge direction (max)
             select case (k)
             case (1); a1 = 2; a2 = 3 ! orthogonal directions to edge direction
             case (2); a1 = 1; a2 = 3 ! orthogonal directions to edge direction
             case (3); a1 = 1; a2 = 2 ! orthogonal directions to edge direction
             end select
             TF_edge(3) = abs(m%g(i)%c(a1)%hmin-m%g(j)%c(a1)%hmax).lt.tol*m%dhmin(a1) ! min/max (a1)
             TF_edge(4) = abs(m%g(i)%c(a1)%hmax-m%g(j)%c(a1)%hmin).lt.tol*m%dhmin(a1) ! max/min (a1)
             TF_edge(5) = abs(m%g(i)%c(a2)%hmin-m%g(j)%c(a2)%hmax).lt.tol*m%dhmin(a2) ! min/max (a2)
             TF_edge(6) = abs(m%g(i)%c(a2)%hmax-m%g(j)%c(a2)%hmin).lt.tol*m%dhmin(a2) ! max/min (a2)
                 if (TF_edge(1).and.TF_edge(2).and.TF_edge(3).and.TF_edge(5)) then
               m%g(i)%st_edge%minmin(k) = .true.
               m%g(i)%st_edge%minmin_id(k) = j
               m%g(j)%st_edge%maxmax(k) = .true.
               m%g(j)%st_edge%maxmax_id(k) = i
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(3).and.TF_edge(6)) then
               m%g(i)%st_edge%minmax(k) = .true.
               m%g(i)%st_edge%minmax_id(k) = j
               m%g(j)%st_edge%maxmin(k) = .true.
               m%g(j)%st_edge%maxmin_id(k) = i
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(4).and.TF_edge(5)) then
               m%g(i)%st_edge%maxmin(k) = .true.
               m%g(i)%st_edge%maxmin_id(k) = j
               m%g(j)%st_edge%minmax(k) = .true.
               m%g(j)%st_edge%minmax_id(k) = i
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(4).and.TF_edge(6)) then
               m%g(i)%st_edge%maxmax(k) = .true.
               m%g(i)%st_edge%maxmax_id(k) = j
               m%g(j)%st_edge%minmin(k) = .true.
               m%g(j)%st_edge%minmin_id(k) = i
             endif
           endif
         enddo; enddo; enddo
       end subroutine

       subroutine patch_Edges_adjoin_face(m) ! 12 edges per grid
         ! Remove edges that do not adjoin to 4 faces
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,k,a1,a2
         logical :: TF
         do k=1,3; do i=1,m%s
             select case (k)
             case (1); a1 = 2; a2 = 3 ! orthogonal directions to edge direction
             case (2); a1 = 1; a2 = 3 ! orthogonal directions to edge direction
             case (3); a1 = 1; a2 = 2 ! orthogonal directions to edge direction
             end select
             TF = (m%g(i)%st_face%hmin(a1)).and.(m%g(i)%st_face%hmin(a2))
             if (.not.TF) m%g(i)%st_edge%minmin(k) = .false.

             TF = (m%g(i)%st_face%hmin(a1)).and.(m%g(i)%st_face%hmax(a2))
             if (.not.TF) m%g(i)%st_edge%minmax(k) = .false.

             TF = (m%g(i)%st_face%hmax(a1)).and.(m%g(i)%st_face%hmin(a2))
             if (.not.TF) m%g(i)%st_edge%minmax(k) = .false.

             TF = (m%g(i)%st_face%hmax(a1)).and.(m%g(i)%st_face%hmax(a2))
             if (.not.TF) m%g(i)%st_edge%maxmax(k) = .false.
         enddo; enddo
       end subroutine

       subroutine patch_Corners(m,tol) ! 8 corners per grid
         ! Find grids who share an corner, but NOT an edge.
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         integer :: i,j,k,a1,a2
         logical,dimension(6) :: TF_corner
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_corner(1) = abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmax).lt.tol*m%dhmin(k) ! Edge direction (min)
             TF_corner(2) = abs(m%g(i)%c(k)%hmax-m%g(j)%c(k)%hmin).lt.tol*m%dhmin(k) ! Edge direction (max)
             select case (k)
             case (1); a1 = 2; a2 = 3 ! orthogonal directions to contact face
             case (2); a1 = 1; a2 = 3 ! orthogonal directions to contact face
             case (3); a1 = 1; a2 = 2 ! orthogonal directions to contact face
             end select
             TF_corner(3) = abs(m%g(i)%c(a1)%hmin-m%g(j)%c(a1)%hmax).lt.tol*m%dhmin(a1)
             TF_corner(4) = abs(m%g(i)%c(a2)%hmin-m%g(j)%c(a2)%hmax).lt.tol*m%dhmin(a2)
             TF_corner(5) = abs(m%g(i)%c(a2)%hmax-m%g(j)%c(a2)%hmin).lt.tol*m%dhmin(a2)
             TF_corner(6) = abs(m%g(i)%c(a1)%hmax-m%g(j)%c(a1)%hmin).lt.tol*m%dhmin(a1)
                 if (TF_corner(3).and.TF_corner(4).and.TF_corner(1)) then
               m%g(i)%st_corner%minmin = .true.
               m%g(i)%st_corner%minmin_id = j
               m%g(j)%st_corner%maxmax = .true.
               m%g(j)%st_corner%maxmax_id = i
             elseif (TF_corner(5).and.TF_corner(6).and.TF_corner(1)) then
               m%g(i)%st_corner%minmax(k) = .true.
               m%g(i)%st_corner%minmax_id(k) = j
               m%g(j)%st_corner%maxmin(k) = .true.
               m%g(j)%st_corner%maxmin_id(k) = i
             elseif (TF_corner(3).and.TF_corner(5).and.TF_corner(2)) then
               m%g(i)%st_corner%maxmin(k) = .true.
               m%g(i)%st_corner%maxmin_id(k) = j
               m%g(j)%st_corner%minmax(k) = .true.
               m%g(j)%st_corner%minmax_id(k) = i
             elseif (TF_corner(4).and.TF_corner(6).and.TF_corner(2)) then
               m%g(i)%st_corner%maxmax = .true.
               m%g(i)%st_corner%maxmax_id = j
               m%g(j)%st_corner%minmin = .true.
               m%g(j)%st_corner%minmin_id = i
             endif
           endif
         enddo; enddo; enddo
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

#ifdef _DEBUG_COORDINATES_
       subroutine checkmesh(m)
         implicit none
         type(mesh),intent(in) :: m
         integer :: i
         do i = 1,m%s; call checkGrid(m%g(i)) ;enddo
       end subroutine
#endif

       subroutine exportmesh_all(m,dir,name)
         implicit none
         type(mesh), intent(in) :: m
         character(len=*),intent(in) :: dir,name
         integer :: i,NU
         NU = newAndOpen(dir,name)
         do i = 1,m%s; call export_all(m%g(i),NU) ;enddo
         call closeandMessage(NU,name,dir)
       end subroutine

       subroutine print_mesh(m)
         implicit none
         type(mesh), intent(in) :: m
         call export(m,6)
       end subroutine

       subroutine export_mesh(m,un)
         implicit none
         type(mesh), intent(in) :: m
         integer,intent(in) :: un
         integer :: i
         write(un,*) ' ************* mesh ************* '
         do i = 1,m%s
           write(un,*) 'grid ID = ',i
           call export(m%g(i),un)
           if (m%s.gt.1) call export_stitches(m%g(i),un)
           write(un,*) ' -------------------------------- '
         enddo
         if (m%s.gt.1) then
           write(un,*) 's = ',m%s
           write(un,*) 'N_cells = ',m%N_cells
           write(un,*) 'volume = ',m%volume
           write(un,*) 'min/max(h)_x = ',(/m%hmin(1),m%hmax(1)/)
           write(un,*) 'min/max(h)_y = ',(/m%hmin(2),m%hmax(2)/)
           write(un,*) 'min/max(h)_z = ',(/m%hmin(3),m%hmax(3)/)
           write(un,*) 'min/max(dh)_x = ',(/m%dhmin(1),m%dhmax(1)/)
           write(un,*) 'min/max(dh)_y = ',(/m%dhmin(2),m%dhmax(2)/)
           write(un,*) 'min/max(dh)_z = ',(/m%dhmin(3),m%dhmax(3)/)
         endif
         write(un,*) ' ******************************** '
       end subroutine

       end module