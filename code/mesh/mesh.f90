       module mesh_mod
      ! Pre-processor directives: (_DEBUG_MESH_)
       use current_precision_mod
       use simple_int_tensor_mod
       use IO_tools_mod
       use grid_mod
       use block_mod
       use grid_genHelper_mod
       use coordinates_mod
       use face_edge_corner_indexing_mod
       use stitch_mod
       use GF_mod
       implicit none

       private
       public :: mesh
       public :: init,add,delete
       public :: export,import
       public :: print,display
       public :: patch
       public :: restrict,restrict_x,restrict_xy
       public :: initProps
       public :: init_surface
       public :: mirror_about_hmin
       public :: mirror_about_hmax
       public :: compare

       public :: restrict
       public :: prolongate

#ifdef _DEBUG_COORDINATES_
      public :: checkmesh
#endif

       type mesh
         type(block),dimension(:),allocatable :: B
         ! Properties
         integer :: s       ! Number of grids
         integer,dimension(3) :: N_cells ! Number of cells, for export
         integer :: N_cells_tot ! Total number of cells
         real(cp) :: volume
         real(cp),dimension(3) :: hmax,hmin
         real(cp),dimension(3) :: dhmax,dhmin
         real(cp) :: dhmax_max,dhmin_min
         logical :: defined = .false.
         logical :: plane_x = .false.
         logical :: plane_y = .false.
         logical :: plane_z = .false.
         logical :: plane_xyz = .false.
         type(simple_int_tensor),dimension(3) :: int_tensor
       end type

       interface init;                 module procedure init_grid;              end interface
       interface init;                 module procedure initmeshCopy;           end interface
       interface add;                  module procedure addGrid;                end interface
       interface delete;               module procedure deletemesh;             end interface
       interface initProps;            module procedure initProps_mesh;         end interface

       interface patch;                module procedure patch_grids;            end interface

       interface restrict;             module procedure restrictmesh1;          end interface
       interface restrict;             module procedure restrictmesh3;          end interface
       interface restrict_x;           module procedure restrictmesh_x;         end interface
       interface restrict_xy;          module procedure restrictmesh_xy;        end interface

       interface restrict;             module procedure restrict_dir_m;         end interface
       interface prolongate;           module procedure prolongate_dir_m;       end interface

       interface mirror_about_hmin;    module procedure mirror_about_hmin_m;    end interface
       interface mirror_about_hmax;    module procedure mirror_about_hmax_m;    end interface

       interface print;                module procedure print_mesh;             end interface
       interface display;              module procedure display_mesh;           end interface
       interface export;               module procedure export_mesh;            end interface
       interface import;               module procedure import_mesh;            end interface
       interface export;               module procedure export_wrapper;         end interface
       interface import;               module procedure import_wrapper;         end interface

       interface compare;              module procedure compare_mesh;           end interface

       contains

       subroutine init_grid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         call delete(m)
         allocate(m%B(1))
         call init(m%B(1),g)
         m%s = 1
         call initProps(m)
         call insist_allocated_mesh(m,'init_grid')
         m%defined = .true.
       end subroutine

       subroutine init_surface(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         type(gridGenerator) :: gg
         integer :: i,face
         call delete(m)
         do i=1,m_in%s; do face=1,6
           call init(gg%g,m_in%B(i)%g)
           call get_boundary_face(gg,face)
           if ((i.eq.1).and.(face.eq.1)) then; call init(m,gg%g)
           else;                               call add(m,gg%g)
           endif
         enddo; enddo
         call delete(gg)
         call initProps(m)
         call insist_allocated_mesh(m,'init_surface')
       end subroutine

       subroutine addGrid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         type(mesh) :: temp
         integer :: i
         if (allocated(m%B)) then
           if (m%s.lt.1) stop 'Error: allocated mesh but size<1 in addGrid mesh.f90'
           call init(temp,m)
           call delete(m)
           m%s = temp%s + 1
           allocate(m%B(m%s))
           do i=1,temp%s; call init(m%B(i),temp%B(i)); enddo
           call init(m%B(m%s),g)
           call delete(temp)
         else
           allocate(m%B(1))
           call init(m%B(1),g); m%s = 1
         endif
         call initProps(m)
         call insist_allocated_mesh(m,'addGrid')
         m%defined = .true.
       end subroutine

       subroutine deletemesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         if (allocated(m%B)) then
           do i=1,m%s; call delete(m%B(i)%g) ;enddo; deallocate(m%B)
         endif
         m%s = 0
         m%defined = .false.
       end subroutine

       subroutine remove_stitches(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         do i=1,m%s
         enddo
       end subroutine

       subroutine mirror_about_hmin_m(m,dir)
         implicit none
         type(mesh),intent(inout) :: m
         integer,intent(in) :: dir
         integer :: i
         do i=1,m%s; call mirror_about_hmin(m%B(i),dir); enddo
       end subroutine

       subroutine mirror_about_hmax_m(m,dir)
         implicit none
         type(mesh),intent(inout) :: m
         integer,intent(in) :: dir
         integer :: i
         do i=1,m%s; call mirror_about_hmax(m%B(i),dir); enddo
       end subroutine

       subroutine initmeshCopy(m,m_in)
         implicit none
         type(mesh),intent(inout) :: m
         type(mesh),intent(in) :: m_in
         integer :: i
         call insist_allocated_mesh(m_in,'initmeshCopy')
         call delete(m)
         if (m_in%s.lt.1) stop 'Error: mesh allocated but size<1 in initmeshCopy in mesh.f90'
         m%s = m_in%s
         allocate(m%B(m_in%s))
         do i=1,m_in%s; call init(m%B(i),m_in%B(i)); enddo
         call initProps(m)
         m%defined = m_in%defined
       end subroutine

       subroutine initProps_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j
         call insist_allocated_mesh(m,'initProps_mesh')
         do i=1,m%s; call init_FEC(m%B(i)); enddo
         if (m%s.gt.1) then
           do j=1,3
             m%hmin(j)  = minval( (/(m%B(i)%g%c(j)%hmin  , i=2,m%s)/) )
             m%dhmin(j) = minval( (/(m%B(i)%g%c(j)%dhmin , i=2,m%s)/) )
             m%hmax(j)  = maxval( (/(m%B(i)%g%c(j)%hmax  , i=2,m%s)/) )
             m%dhmax(j) = maxval( (/(m%B(i)%g%c(j)%dhmax , i=2,m%s)/) )
           enddo
         else
           do j=1,3
             m%hmin(j)  = m%B(1)%g%c(j)%hmin
             m%dhmin(j) = m%B(1)%g%c(j)%dhmin
             m%hmax(j)  = m%B(1)%g%c(j)%hmax
             m%dhmax(j) = m%B(1)%g%c(j)%dhmax
           enddo
         endif
         m%N_cells = 0; m%N_cells_tot = 0; m%volume = 0.0_cp
         do i=1,m%s
           m%volume = m%volume + m%B(i)%g%volume
           do j=1,3
             m%N_cells(j) = m%N_cells(j) + m%B(i)%g%c(j)%N
           enddo
           m%N_cells_tot = m%N_cells_tot + m%B(i)%g%c(1)%N*m%B(i)%g%c(2)%N*m%B(i)%g%c(3)%N
         enddo
         m%dhmin_min = minval((/m%dhmin(1),m%dhmin(2),m%dhmin(3)/))
         m%dhmax_max = maxval((/m%dhmax(1),m%dhmax(2),m%dhmax(3)/))

         m%plane_x = all((/(m%B(i)%g%c(1)%N.eq.1,i=1,m%s)/))
         m%plane_y = all((/(m%B(i)%g%c(2)%N.eq.1,i=1,m%s)/))
         m%plane_z = all((/(m%B(i)%g%c(3)%N.eq.1,i=1,m%s)/))
         m%plane_xyz = any((/m%plane_x,m%plane_y,m%plane_z/))
         do i=1,3; call init(m%int_tensor(i),i); enddo
       end subroutine

       subroutine patch_grids(m)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp) :: tol
         call insist_allocated_mesh(m,'patch_grids')
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
             TF_face(1) = abs(m%B(i)%g%c(k)%hmin-m%B(j)%g%c(k)%hmax).lt.tol*m%dhmin(k) ! Contact face
             a = adj_dir_given_dir(k)
             TF_face(2) = abs(m%B(i)%g%c(a(1))%hmin-m%B(j)%g%c(a(1))%hmin).lt.tol*m%dhmin(a(1)) ! Adjacent face 1 hmin
             TF_face(3) = abs(m%B(i)%g%c(a(1))%hmax-m%B(j)%g%c(a(1))%hmax).lt.tol*m%dhmin(a(1)) ! Adjacent face 1 hmax
             TF_face(4) = abs(m%B(i)%g%c(a(2))%hmin-m%B(j)%g%c(a(2))%hmin).lt.tol*m%dhmin(a(2)) ! Adjacent face 2 hmin
             TF_face(5) = abs(m%B(i)%g%c(a(2))%hmax-m%B(j)%g%c(a(2))%hmax).lt.tol*m%dhmin(a(2)) ! Adjacent face 2 hmax

             if (all(TF_face)) then
               f = normal_faces_given_dir(k)
             endif
           endif
         enddo; enddo; enddo
         do k=1,3; do i=1,m%s
            f = normal_faces_given_dir(k)
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
             TF_edge(1) = abs(m%B(i)%g%c(k)%hmin-m%B(j)%g%c(k)%hmin).lt.tol*m%dhmin(k) ! Edge direction (min)
             TF_edge(2) = abs(m%B(i)%g%c(k)%hmax-m%B(j)%g%c(k)%hmax).lt.tol*m%dhmin(k) ! Edge direction (max)
             a = adj_dir_given_dir(k)
             TF_edge(3) = abs(m%B(i)%g%c(a(1))%hmin-m%B(j)%g%c(a(1))%hmax).lt.tol*m%dhmin(a(1)) ! min/max (a(1))
             TF_edge(4) = abs(m%B(i)%g%c(a(1))%hmax-m%B(j)%g%c(a(1))%hmin).lt.tol*m%dhmin(a(1)) ! max/min (a(1))
             TF_edge(5) = abs(m%B(i)%g%c(a(2))%hmin-m%B(j)%g%c(a(2))%hmax).lt.tol*m%dhmin(a(2)) ! min/max (a(2))
             TF_edge(6) = abs(m%B(i)%g%c(a(2))%hmax-m%B(j)%g%c(a(2))%hmin).lt.tol*m%dhmin(a(2)) ! max/min (a(2))
             e = edges_given_dir(k)
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
         enddo; enddo
       end subroutine

       function compare_mesh(a,b) result(L_all)
         implicit none
         type(mesh),intent(in) :: a,b
         logical,dimension(8) :: L
         integer :: i
         logical :: L_all
         real(cp) :: tol
         tol = 10.0_cp**(-12.0_cp)
         L(1) = all((/(abs(a%hmax(i)-b%hmax(i)).lt.tol,i=1,3)/))
         L(2) = all((/(abs(a%hmin(i)-b%hmin(i)).lt.tol,i=1,3)/))
         L(3) = all((/(abs(a%dhmax(i)-b%dhmax(i)).lt.tol,i=1,3)/))
         L(4) = all((/(abs(a%dhmin(i)-b%dhmin(i)).lt.tol,i=1,3)/))
         L(5) = abs(a%volume-b%volume).lt.tol
         L(6) = all((/(a%N_cells(i)-b%N_cells(i).eq.0,i=1,3)/))
         L(7) = a%N_cells_tot-b%N_cells_tot.eq.0
         L(8) = a%s-b%s.eq.0
         L_all = all(L)
       end function

       ! ------------------- restrict (for multimesh) --------------

       subroutine restrictmesh1(rm,m,dir)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer :: i
         call insist_allocated_mesh(m,'restrictmesh1 (1)')
         call insist_allocated_mesh(rm,'restrictmesh1 (2)')
         do i = 1,m%s; call restrict(rm%B(i)%g%c(dir),m%B(i)%g%c(dir)) ;enddo
       end subroutine

       subroutine restrictmesh3(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer :: i
         call insist_allocated_mesh(m,'restrictmesh3 (1)')
         call insist_allocated_mesh(rm,'restrictmesh3 (2)')
         do i = 1,m%s; call restrict(rm%B(i)%g,m%B(i)%g) ;enddo
       end subroutine

       subroutine restrictmesh_x(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer :: i
         call insist_allocated_mesh(m,'restrictmesh_x (1)')
         call insist_allocated_mesh(rm,'restrictmesh_x (2)')
         do i = 1,m%s; call restrict(rm%B(i)%g%c(1),m%B(i)%g%c(1)) ;enddo
       end subroutine

       subroutine restrictmesh_xy(rm,m)
         type(mesh),intent(inout) :: rm
         type(mesh),intent(in) :: m
         integer :: i
         call insist_allocated_mesh(m,'restrictmesh_xy (1)')
         call insist_allocated_mesh(rm,'restrictmesh_xy (2)')
         do i = 1,m%s; call restrict(rm%B(i)%g%c(1),m%B(i)%g%c(1)) ;enddo
         do i = 1,m%s; call restrict(rm%B(i)%g%c(2),m%B(i)%g%c(2)) ;enddo
       end subroutine

       subroutine restrict_dir_m(m,dir)
         type(mesh),intent(inout) :: m
         integer,intent(in) :: dir
         integer :: i
         do i = 1,m%s; call restrict(m%B(i),dir) ;enddo
       end subroutine

       subroutine prolongate_dir_m(m,dir)
         type(mesh),intent(inout) :: m
         integer,intent(in) :: dir
         integer :: i
         do i = 1,m%s; call prolongate(m%B(i),dir) ;enddo
         call initProps(m)
       end subroutine

       ! ---------------------------------------------- check mesh

#ifdef _DEBUG_COORDINATES_
       subroutine checkmesh(m)
         implicit none
         type(mesh),intent(in) :: m
         integer :: i
         do i = 1,m%s; call checkGrid(m%B(i)%g) ;enddo
       end subroutine
#endif

       subroutine export_mesh(m,un)
         implicit none
         type(mesh), intent(in) :: m
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'N_grids'
         write(un,*) m%s
         do i = 1,m%s
           write(un,*) ' ------------- mesh ------------- grid ID = ',i
           call export(m%B(i)%g,un)
         enddo
       end subroutine

       subroutine import_mesh(m,un)
         implicit none
         type(mesh), intent(inout) :: m
         integer,intent(in) :: un
         integer :: i
         call delete(m)
         read(un,*)
         read(un,*) m%s
         allocate(m%B(m%s))
         do i = 1,m%s
           read(un,*);
           call import(m%B(i)%g,un)
         enddo
         call initProps(m)
       end subroutine

       subroutine export_wrapper(m,dir,name)
         implicit none
         type(mesh), intent(in) :: m
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(m,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine import_wrapper(m,dir,name)
         implicit none
         type(mesh), intent(inout) :: m
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = open_to_read(dir,name)
         call import(m,un)
         call close_and_message(un,dir,name)
       end subroutine

       subroutine print_mesh(m)
         implicit none
         type(mesh), intent(in) :: m
         call display(m,6)
       end subroutine

       subroutine display_mesh(m,un)
         implicit none
         type(mesh), intent(in) :: m
         integer,intent(in) :: un
         integer :: i
         if (un.ne.6) then
           do i = 1,m%s
             write(un,*) ' ------------- mesh ------------- grid ID = ',i
             call display(m%B(i)%g,un)
           enddo
           write(un,*) ' -------------------------------- '
         else
           if (m%s.lt.5) then
             do i = 1,m%s
               write(un,*) ' ------------- mesh ------------- grid ID = ',i
                 call display(m%B(i)%g,un)
             enddo
           write(un,*) ' -------------------------------- '
           else
           write(un,*) 'Many grids. To see details, see info file or change limits in mesh.f90'
           endif
         endif

         if (m%s.gt.1) then
           write(un,*) ' *********** mesh props *********** '
           write(un,*) 's,volume = ',m%s,m%volume
           write(un,*) 'N_cells_tot,N_cells = ',m%N_cells_tot,m%N_cells
           write(un,*) 'min/max(h)_x = ',(/m%hmin(1),m%hmax(1)/)
           write(un,*) 'min/max(h)_y = ',(/m%hmin(2),m%hmax(2)/)
           write(un,*) 'min/max(h)_z = ',(/m%hmin(3),m%hmax(3)/)
           write(un,*) 'min/max(dh)_x = ',(/m%dhmin(1),m%dhmax(1)/)
           write(un,*) 'min/max(dh)_y = ',(/m%dhmin(2),m%dhmax(2)/)
           write(un,*) 'min/max(dh)_z = ',(/m%dhmin(3),m%dhmax(3)/)
           write(un,*) ' ********************************** '
         endif
       end subroutine

       subroutine insist_allocated_mesh(m,caller)
         implicit none
         type(mesh),intent(in) :: m
         character(len=*),intent(in) :: caller
         if (.not.allocated(m%B)) then
          write(*,*) 'Error: mesh not allocated in '//caller//' in mesh.f90'
          stop 'Done'
         elseif (size(m%B).ne.m%s) then
          write(*,*) 'Error: mesh size not correct in '//caller//' in mesh.f90'
          stop 'Done'
         endif
       end subroutine

       end module