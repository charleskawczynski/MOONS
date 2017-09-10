       module mesh_extend_mod
       use mesh_mod
      ! Pre-processor directives: (_DEBUG_MESH_)
       use current_precision_mod
       use IO_tools_mod
       use grid_mod
       use block_mod
       use block_extend_mod
       use grid_extend_mod
       use block_extend_mod
       use coordinates_mod
       use coordinates_extend_mod
       use face_edge_corner_indexing_mod
       use stitch_mod
       use GF_mod
       ! use simple_int_tensor_extend_mod
       use mesh_props_mod
       implicit none

       private
       public :: mesh
       public :: init,delete,display,print,export,import
       public :: display_info,print_info

       public :: add
       public :: patch

       public :: restrict_x,restrict_xy
       public :: restrict
       public :: prolongate

       public :: init_props
       public :: init_apply_BC_order
       public :: mirror_about_hmin
       public :: mirror_about_hmax
       public :: compare


#ifdef _DEBUG_COORDINATES_
      public :: checkmesh
#endif

       interface init;                module procedure init_grid;               end interface
       interface display_info;        module procedure display_mesh_info;       end interface
       interface print_info;          module procedure print_mesh_info;         end interface

       interface add;                 module procedure addGrid;                 end interface
       interface init_props;          module procedure init_props_mesh;         end interface
       interface init_apply_BC_order; module procedure init_apply_BC_order_mesh;end interface
       interface init_FEC;            module procedure init_FEC_mesh;           end interface
       interface patch;               module procedure patch_blocks;             end interface

       interface restrict;            module procedure restrictmesh1;           end interface
       interface restrict;            module procedure restrictmesh3;           end interface
       interface restrict;            module procedure restrict_dir_m;          end interface
       interface prolongate;          module procedure prolongate_dir_m;        end interface

       interface mirror_about_hmin;   module procedure mirror_about_hmin_m;     end interface
       interface mirror_about_hmax;   module procedure mirror_about_hmax_m;     end interface

       interface compare;             module procedure compare_mesh;            end interface

       contains

       subroutine init_grid(m,g)
         implicit none
         type(mesh),intent(inout) :: m
         type(grid),intent(in) :: g
         call delete(m)
         allocate(m%B(1))
         call init(m%B(1),g)
         m%s = 1
         call init_props(m)
         call insist_allocated_mesh(m,'init_grid')
         m%defined = .true.
       end subroutine

       subroutine display_mesh_info(m,un)
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
           write(un,*) 's,volume = ',m%s,m%MP%volume
           write(un,*) 'N_cells_tot,N_cells = ',m%MP%N_cells_tot,m%MP%N_cells
           write(un,*) 'min/max(h)_x = ',(/m%MP%hmin(1),m%MP%hmax(1)/)
           write(un,*) 'min/max(h)_y = ',(/m%MP%hmin(2),m%MP%hmax(2)/)
           write(un,*) 'min/max(h)_z = ',(/m%MP%hmin(3),m%MP%hmax(3)/)
           write(un,*) 'min/max(dh)_x = ',(/m%MP%dhmin(1),m%MP%dhmax(1)/)
           write(un,*) 'min/max(dh)_y = ',(/m%MP%dhmin(2),m%MP%dhmax(2)/)
           write(un,*) 'min/max(dh)_z = ',(/m%MP%dhmin(3),m%MP%dhmax(3)/)
           write(un,*) ' ********************************** '
         endif
       end subroutine

       subroutine print_mesh_info(m)
         implicit none
         type(mesh), intent(in) :: m
         call display(m,6)
       end subroutine

       ! **************************************************************
       ! **************************************************************
       ! **************************************************************

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
           call init(m%B(1),g)
           m%s = 1
         endif
         call init_props(m)
         call insist_allocated_mesh(m,'addGrid')
         m%defined = .true.
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

       subroutine init_props_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i,j
         call init_FEC(m)
         call insist_allocated_mesh(m,'init_props_mesh')
         if (m%s.gt.1) then
           do j=1,3
             m%MP%hmin(j)  = minval( (/(m%B(i)%g%c(j)%hmin  , i=2,m%s)/) )
             m%MP%dhmin(j) = minval( (/(m%B(i)%g%c(j)%dhmin , i=2,m%s)/) )
             m%MP%hmax(j)  = maxval( (/(m%B(i)%g%c(j)%hmax  , i=2,m%s)/) )
             m%MP%dhmax(j) = maxval( (/(m%B(i)%g%c(j)%dhmax , i=2,m%s)/) )
           enddo
         else
           do j=1,3
             m%MP%hmin(j)  = m%B(1)%g%c(j)%hmin
             m%MP%dhmin(j) = m%B(1)%g%c(j)%dhmin
             m%MP%hmax(j)  = m%B(1)%g%c(j)%hmax
             m%MP%dhmax(j) = m%B(1)%g%c(j)%dhmax
           enddo
         endif
         m%MP%N_cells = 0; m%MP%N_cells_tot = 0; m%MP%volume = 0.0_cp
         do i=1,m%s; m%MP%volume = m%MP%volume + m%B(i)%g%volume; enddo
         do i=1,m%s; call init_vol(m%B(i)); enddo
         do i=1,m%s;do j=1,3; m%MP%N_cells(j) = m%MP%N_cells(j) + m%B(i)%g%c(j)%N; enddo; enddo
         do i=1,m%s;m%MP%N_cells_tot=m%MP%N_cells_tot+m%B(i)%g%c(1)%N*m%B(i)%g%c(2)%N*m%B(i)%g%c(3)%N;enddo

         m%MP%dhmin_min = minval((/m%MP%dhmin(1),m%MP%dhmin(2),m%MP%dhmin(3)/))
         m%MP%dhmax_max = maxval((/m%MP%dhmax(1),m%MP%dhmax(2),m%MP%dhmax(3)/))

         m%MP%plane(1) = all((/(m%B(i)%g%c(1)%N.eq.1,i=1,m%s)/))
         m%MP%plane(2) = all((/(m%B(i)%g%c(2)%N.eq.1,i=1,m%s)/))
         m%MP%plane(3) = all((/(m%B(i)%g%c(3)%N.eq.1,i=1,m%s)/))
         m%MP%plane_any = any(m%MP%plane)
         do i=1,3
          ! call init(m%MP%int_tensor(i),i)
          m%MP%int_tensor(i)%eye = eye_given_dir(i)
         enddo
       end subroutine

       subroutine init_apply_BC_order_mesh(m,apply_BC_order)
         implicit none
         type(mesh),intent(inout) :: m
         integer,dimension(6),intent(in) :: apply_BC_order
         integer :: i
         do i=1,m%s; call init_apply_BC_order(m%B(i),apply_BC_order); enddo
       end subroutine

       subroutine init_FEC_mesh(m)
         implicit none
         type(mesh),intent(inout) :: m
         integer :: i
         call insist_allocated_mesh(m,'init_FEC_mesh')
         do i=1,m%s; call init_FEC(m%B(i)); enddo
       end subroutine

       subroutine patch_blocks(m)
         implicit none
         type(mesh),intent(inout) :: m
         real(cp) :: tol
         call insist_allocated_mesh(m,'patch_blocks')
         call remove_stitches(m)
         if (m%s.gt.1) then
           tol = 0.01_cp
           ! Remove all patches first
           call init_props(m) ! Need dhmin etc.
           call patch_Faces(m,tol)
           call patch_Edges(m,tol)
           call patch_Corners(m,tol)
         endif
       end subroutine

       subroutine patch_Faces(m,tol) ! 6 faces per grid
         ! Find blocks that share a face.
         implicit none
         type(mesh),intent(inout) :: m
         real(cp),intent(in) :: tol
         integer :: i,j,k
         integer,dimension(2) :: f,a
         logical,dimension(5) :: TF_face
         do k=1,3; do i=1,m%s; do j=1,m%s
           if (i.ne.j) then
             TF_face(1) = abs(m%B(i)%g%c(k)%hmin-m%B(j)%g%c(k)%hmax).lt.tol*m%MP%dhmin(k) ! Contact face
             a = adj_dir_given_dir(k)
             TF_face(2) = abs(m%B(i)%g%c(a(1))%hmin-m%B(j)%g%c(a(1))%hmin).lt.tol*m%MP%dhmin(a(1)) ! Adjacent face 1 hmin
             TF_face(3) = abs(m%B(i)%g%c(a(1))%hmax-m%B(j)%g%c(a(1))%hmax).lt.tol*m%MP%dhmin(a(1)) ! Adjacent face 1 hmax
             TF_face(4) = abs(m%B(i)%g%c(a(2))%hmin-m%B(j)%g%c(a(2))%hmin).lt.tol*m%MP%dhmin(a(2)) ! Adjacent face 2 hmin
             TF_face(5) = abs(m%B(i)%g%c(a(2))%hmax-m%B(j)%g%c(a(2))%hmax).lt.tol*m%MP%dhmin(a(2)) ! Adjacent face 2 hmax

             if (all(TF_face)) then
               f = normal_faces_given_dir(k)
               ! call stitch_faces(m%B(i),m%B(j),f(1),f(2))
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
             TF_edge(1) = abs(m%B(i)%g%c(k)%hmin-m%B(j)%g%c(k)%hmin).lt.tol*m%MP%dhmin(k) ! Edge direction (min)
             TF_edge(2) = abs(m%B(i)%g%c(k)%hmax-m%B(j)%g%c(k)%hmax).lt.tol*m%MP%dhmin(k) ! Edge direction (max)
             a = adj_dir_given_dir(k)
             TF_edge(3) = abs(m%B(i)%g%c(a(1))%hmin-m%B(j)%g%c(a(1))%hmax).lt.tol*m%MP%dhmin(a(1)) ! min/max (a(1))
             TF_edge(4) = abs(m%B(i)%g%c(a(1))%hmax-m%B(j)%g%c(a(1))%hmin).lt.tol*m%MP%dhmin(a(1)) ! max/min (a(1))
             TF_edge(5) = abs(m%B(i)%g%c(a(2))%hmin-m%B(j)%g%c(a(2))%hmax).lt.tol*m%MP%dhmin(a(2)) ! min/max (a(2))
             TF_edge(6) = abs(m%B(i)%g%c(a(2))%hmax-m%B(j)%g%c(a(2))%hmin).lt.tol*m%MP%dhmin(a(2)) ! max/min (a(2))
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
         L(1) = all((/(abs(a%MP%hmax(i)-b%MP%hmax(i)).lt.tol,i=1,3)/))
         L(2) = all((/(abs(a%MP%hmin(i)-b%MP%hmin(i)).lt.tol,i=1,3)/))
         L(3) = all((/(abs(a%MP%dhmax(i)-b%MP%dhmax(i)).lt.tol,i=1,3)/))
         L(4) = all((/(abs(a%MP%dhmin(i)-b%MP%dhmin(i)).lt.tol,i=1,3)/))
         L(5) = abs(a%MP%volume-b%MP%volume).lt.tol
         L(6) = all((/(a%MP%N_cells(i)-b%MP%N_cells(i).eq.0,i=1,3)/))
         L(7) = a%MP%N_cells_tot-b%MP%N_cells_tot.eq.0
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
         call init_props(m)
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