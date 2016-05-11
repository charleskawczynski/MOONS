       module mesh_mod
      ! Pre-processor directives: (_DEBUG_MESH_)
       use current_precision_mod
       use IO_tools_mod
       use grid_mod
       use grid_genHelper_mod
       use coordinates_mod
       use face_edge_corner_indexing_mod
       use stitch_mod
       use RF_mod
       implicit none

       private
       public :: mesh
       public :: init,add,delete
       public :: print,export ! import
       public :: patch
       public :: restrict,restrict_x,restrict_xy
       public :: initProps
       public :: init_surface

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
         real(cp) :: dhmax_max,dhmin_min
         logical :: plane_x,plane_y,plane_z
         type(realField),dimension(:),allocatable :: vol
       end type

       interface init;           module procedure init_grid;           end interface
       interface init;           module procedure initmeshCopy;        end interface
       interface add;            module procedure addGrid;             end interface
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
         integer :: i,k
         call delete(m)
         allocate(m%g(1))
         call init(m%g(1),g); m%s = 1
         call initProps(m)
         do i=1,m%s
           do k=1,6; call delete(m%g(i)%st_faces(i)); enddo
           do k=1,12; call delete(m%g(i)%st_edges(i)); enddo
           do k=1,8; call delete(m%g(i)%st_corners(i)); enddo
         enddo
         if (m%s.ne.size(m%g)) stop 'Error: size incorrect in init_grid in mesh.f90'
       end subroutine

       subroutine init_surface(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(gridGenerator) :: gg
         integer :: i,face
         call delete(m)
         do i=1,m_in%s; do face=1,6
           call init(gg%g,m_in%g(i))
           call get_boundary_face(gg,face)
           if ((i.eq.1).and.(face.eq.1)) then; call init(m,gg%g)
           else;                               call add(m,gg%g)
           endif
         enddo; enddo
         call delete(gg)
         call initProps(m)
         if (m%s.ne.size(m%g)) stop 'Error: size incorrect in init_surface in mesh.f90'
       end subroutine

       subroutine addGrid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         type(mesh) :: temp
         integer :: i
         if (allocated(m%g)) then
           if (m%s.lt.1) stop 'Error: allocated mesh but size<1 in addGrid mesh.f90'
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
         if (m%s.ne.size(m%g)) stop 'Error: size incorrect in addGrid in mesh.f90'
       end subroutine

       subroutine deletemesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         if (allocated(m%vol)) then
           do i=1,m%s; call delete(m%vol(i)); enddo; deallocate(m%vol)
         endif
         if (allocated(m%g)) then
           do i=1,m%s; call delete(m%g(i)) ;enddo; deallocate(m%g)
         endif
         m%s = 0
       end subroutine

       subroutine remove_stitches(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,k
         do i=1,m%s
           do k=1,6; call delete(m%g(i)%st_faces(k)); enddo
           do k=1,12; call delete(m%g(i)%st_edges(k)); enddo
           do k=1,8; call delete(m%g(i)%st_corners(k)); enddo
         enddo
       end subroutine

       subroutine initmeshCopy(m_out,m_in)
         implicit none
         type(mesh),intent(inout) :: m_out
         type(mesh),intent(in) :: m_in
         integer :: i
         if (.not.allocated(m_in%g)) stop 'Error: mesh not allocated in initmeshCopy in mesh.f90'
         if (m_in%s.ne.size(m_in%g)) stop 'Error: size incorrect in addGrid in mesh.f90'
         call delete(m_out)
         if (m_in%s.lt.1) stop 'Error: mesh allocated but size<1 in initmeshCopy in mesh.f90'
         m_out%s = m_in%s
         allocate(m_out%g(m_in%s))
         do i=1,m_in%s; call init(m_out%g(i),m_in%g(i)) ;enddo
         call initProps(m_out)
       end subroutine

       subroutine init_volume(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j,k,t
         if (allocated(m%vol)) then
           do t=1,m%s; call delete(m%vol(t)); enddo
           deallocate(m%vol)
         endif
         allocate(m%vol(m%s))
         do t=1,m%s; call init_CC(m%vol(t),m%g(t)); enddo
         !$OMP PARALLEL DO SHARED(m)
         do t=1,m%s; do k=2,m%g(t)%c(3)%sc-1; do j=2,m%g(t)%c(2)%sc-1; do i=2,m%g(t)%c(1)%sc-1
           m%vol(t)%f(i,j,k) = (m%g(t)%c(1)%dhn(i))*&
                               (m%g(t)%c(2)%dhn(j))*&
                               (m%g(t)%c(3)%dhn(k))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine initProps_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in initProps_mesh in mesh.f90'
         m%s = size(m%g)
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
         m%dhmin_min = minval((/m%dhmin(1),m%dhmin(2),m%dhmin(3)/))
         m%dhmax_max = maxval((/m%dhmax(1),m%dhmax(2),m%dhmax(3)/))

         m%plane_x = all((/(m%g(i)%c(1)%N.eq.1,i=1,m%s)/))
         m%plane_y = all((/(m%g(i)%c(2)%N.eq.1,i=1,m%s)/))
         m%plane_z = all((/(m%g(i)%c(3)%N.eq.1,i=1,m%s)/))
         call init_volume(m)
       end subroutine

       subroutine patch_grids(m)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp) :: tol
         if (.not.allocated(m%g)) stop 'Error: mesh not allocated in patch_grids in mesh.f90'
         if (size(m%g).ne.m%s) stop 'Error: mesh size not correct in patch_grids in mesh.f90'
         call remove_stitches(m)
         if (m%s.gt.1) then
           tol = 0.01_cp
           ! Remove all patches first
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
         integer :: i,j,k
         integer,dimension(2) :: f,a
         logical,dimension(5) :: TF_face
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_face(1) = abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmax).lt.tol*m%dhmin(k) ! Contact face
             a = adj_dir_given_dir(k)
             TF_face(2) = abs(m%g(i)%c(a(1))%hmin-m%g(j)%c(a(1))%hmin).lt.tol*m%dhmin(a(1)) ! Adjacent face 1 hmin
             TF_face(3) = abs(m%g(i)%c(a(1))%hmax-m%g(j)%c(a(1))%hmax).lt.tol*m%dhmin(a(1)) ! Adjacent face 1 hmax
             TF_face(4) = abs(m%g(i)%c(a(2))%hmin-m%g(j)%c(a(2))%hmin).lt.tol*m%dhmin(a(2)) ! Adjacent face 2 hmin
             TF_face(5) = abs(m%g(i)%c(a(2))%hmax-m%g(j)%c(a(2))%hmax).lt.tol*m%dhmin(a(2)) ! Adjacent face 2 hmax

             if (all(TF_face)) then
               f = normal_faces_given_dir(k)
               call init(m%g(i)%st_faces(f(1)),j)
               call init(m%g(j)%st_faces(f(2)),i)
             endif
           endif
         enddo; enddo; enddo
         do k=1,3; do i=1,m%s
            f = normal_faces_given_dir(k)
            call stitch_stencils(m%g(i)%c(k),m%g(i)%st_faces(f(1))%TF,m%g(i)%st_faces(f(2))%TF)
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
         integer :: i,j,k
         integer,dimension(2) :: a
         integer,dimension(4) :: e
         logical,dimension(6) :: TF_edge
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_edge(1) = abs(m%g(i)%c(k)%hmin-m%g(j)%c(k)%hmin).lt.tol*m%dhmin(k) ! Edge direction (min)
             TF_edge(2) = abs(m%g(i)%c(k)%hmax-m%g(j)%c(k)%hmax).lt.tol*m%dhmin(k) ! Edge direction (max)
             a = adj_dir_given_dir(k)
             TF_edge(3) = abs(m%g(i)%c(a(1))%hmin-m%g(j)%c(a(1))%hmax).lt.tol*m%dhmin(a(1)) ! min/max (a(1))
             TF_edge(4) = abs(m%g(i)%c(a(1))%hmax-m%g(j)%c(a(1))%hmin).lt.tol*m%dhmin(a(1)) ! max/min (a(1))
             TF_edge(5) = abs(m%g(i)%c(a(2))%hmin-m%g(j)%c(a(2))%hmax).lt.tol*m%dhmin(a(2)) ! min/max (a(2))
             TF_edge(6) = abs(m%g(i)%c(a(2))%hmax-m%g(j)%c(a(2))%hmin).lt.tol*m%dhmin(a(2)) ! max/min (a(2))
             e = edges_given_dir(k)
                 if (TF_edge(1).and.TF_edge(2).and.TF_edge(3).and.TF_edge(5)) then
               call init(m%g(i)%st_edges(e(1)),j)
               call init(m%g(j)%st_edges(e(4)),i)
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(3).and.TF_edge(6)) then
               call init(m%g(i)%st_edges(e(2)),j)
               call init(m%g(j)%st_edges(e(3)),i)
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(4).and.TF_edge(5)) then
               call init(m%g(i)%st_edges(e(3)),j)
               call init(m%g(j)%st_edges(e(2)),i)
             elseif (TF_edge(1).and.TF_edge(2).and.TF_edge(4).and.TF_edge(6)) then
               call init(m%g(i)%st_edges(e(4)),j)
               call init(m%g(j)%st_edges(e(1)),i)
             endif
           endif
         enddo; enddo; enddo
       end subroutine

       subroutine patch_Edges_adjoin_face(m) ! 12 edges per grid
         ! Remove edges that do not adjoin to 4 faces
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,k
         integer,dimension(2) :: a,f1,f2
         integer,dimension(4) :: e
         do k=1,3; do i=1,m%s
           a = adj_dir_given_dir(k)
           f1 = normal_faces_given_dir(a(1)); f2 = normal_faces_given_dir(a(2)); e = edges_given_dir(k)
           if (.not.(m%g(i)%st_faces(f1(1))%TF.and.(m%g(i)%st_faces(f2(1))%TF))) call delete(m%g(i)%st_edges(e(1)))
           if (.not.(m%g(i)%st_faces(f1(1))%TF.and.(m%g(i)%st_faces(f2(2))%TF))) call delete(m%g(i)%st_edges(e(2)))
           if (.not.(m%g(i)%st_faces(f1(2))%TF.and.(m%g(i)%st_faces(f2(1))%TF))) call delete(m%g(i)%st_edges(e(3)))
           if (.not.(m%g(i)%st_faces(f1(2))%TF.and.(m%g(i)%st_faces(f2(2))%TF))) call delete(m%g(i)%st_edges(e(4)))
         enddo; enddo
       end subroutine

       subroutine patch_Corners(m,tol) ! 8 corners per grid
         ! Find grids who share an corner, but NOT an edge.
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         integer :: i,k
         real(cp) :: suppress_warning
         suppress_warning = tol
         do i=1,m%s; do k=1,8
         call delete(m%g(i)%st_corners(k))
         enddo; enddo
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
         if (un.ne.6) then
           do i = 1,m%s
             write(un,*) ' -------------------------------- grid ID = ',i
             call export(m%g(i),un)
             if (m%s.gt.1) call export_stitches(m%g(i),un)
             write(un,*) ' -------------------------------- '
           enddo
         else
           if (m%s.lt.5) then
             do i = 1,m%s
               write(un,*) ' -------------------------------- grid ID = ',i
                 call export(m%g(i),un)
               if (m%s.gt.1) call export_stitches(m%g(i),un)
               write(un,*) ' -------------------------------- '
             enddo
           else
           write(un,*) 'Many grids. To see details, see info file or change limits in mesh.f90'
           endif
         endif

         if (m%s.gt.1) then
           write(un,*) 's,volume = ',m%s,m%volume
           write(un,*) 'N_cells_tot,N_cells = ',m%N_cells_tot,m%N_cells
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