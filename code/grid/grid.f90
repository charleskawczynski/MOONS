       module grid_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
       use current_precision_mod
       use face_edge_corner_indexing_mod
       use IO_tools_mod
       use coordinates_mod
       implicit none

       private
       public :: grid
       public :: init,delete,display,print,export,import ! Essentials
       public :: restrict,restrict_x,restrict_xy
       public :: initProps
       public :: snip,pop

       public :: get_face_GI
       public :: get_face_b,   get_face_g,   get_face_i
       public :: get_edge_b,   get_edge_g,   get_edge_i
       public :: get_corner_b, get_corner_g, get_corner_i

#ifdef _DEBUG_COORDINATES_
       public :: checkGrid
#endif

       type grid
         type(coordinates),dimension(3) :: c ! hn,hc,dhn,dhc / dhMin,maxRange
         real(cp) :: volume
         logical :: defined
       end type

       interface init;               module procedure initGridCopy;            end interface
       interface init;               module procedure initGrid1;               end interface
       interface init;               module procedure initGrid2;               end interface
       interface delete;             module procedure deleteGrid;              end interface
       interface display;            module procedure display_grid;            end interface
       interface print;              module procedure print_Grid;              end interface
       interface export;             module procedure export_Grid;             end interface
       interface import;             module procedure import_Grid;             end interface

       interface initProps;          module procedure initProps_grid;          end interface
       interface restrict;           module procedure restrictGrid1;           end interface
       interface restrict;           module procedure restrictGrid3;           end interface
       interface restrict_x;         module procedure restrictGrid_x;          end interface
       interface restrict_xy;        module procedure restrictGrid_xy;         end interface

       interface snip;               module procedure snip_grid;               end interface
       interface pop;                module procedure pop_grid;                end interface

       interface get_face_GI;         module procedure get_face_grid_GI;       end interface

       interface get_face_g;         module procedure get_face_grid_g;         end interface
       interface get_face_b;         module procedure get_face_grid_b;         end interface
       interface get_face_i;         module procedure get_face_grid_i;         end interface

       interface get_edge_g;         module procedure get_edge_grid_g;         end interface
       interface get_edge_b;         module procedure get_edge_grid_b;         end interface
       interface get_edge_i;         module procedure get_edge_grid_i;         end interface

       interface get_corner_g;       module procedure get_corner_grid_g;       end interface
       interface get_corner_b;       module procedure get_corner_grid_b;       end interface
       interface get_corner_i;       module procedure get_corner_grid_i;       end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine initGridCopy(g,f)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: f
         integer :: i
         do i=1,3;  call init(g%c(i),f%c(i));                   enddo
         call initProps(g)
         if (.not.g%defined) stop 'Error: tried copying grid that was not fully defined'
       end subroutine

       subroutine initGrid1(g,h,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         integer :: i
         call init(g%c(dir),h,size(h))
         if (all((/(allocated(g%c(i)%hn),i=1,3)/))) then
           call initProps(g)
#ifdef _DEBUG_COORDINATES_
           call checkGrid(g)
#endif
         endif
       end subroutine

       subroutine initGrid2(g,h1,h2,h3)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h1,h2,h3
         call init(g%c(1),h1,size(h1))
         call init(g%c(2),h2,size(h2))
         call init(g%c(3),h3,size(h3))
         call initProps(g)
       end subroutine

       subroutine deleteGrid(g)
         implicit none
         type(grid),intent(inout) :: g
         integer :: i
         do i=1,3;  call delete(g%c(i));          enddo
         call initProps(g)
         ! write(*,*) 'Grid deleted'
       end subroutine

       subroutine display_grid(g,un)
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: un
         write(un,*) 'volume,defined = ',g%volume,g%defined
         write(un,*) 'min/max(h)_x = ',(/g%c(1)%hmin,g%c(1)%hmax/)
         write(un,*) 'min/max(h)_y = ',(/g%c(2)%hmin,g%c(2)%hmax/)
         write(un,*) 'min/max(h)_z = ',(/g%c(3)%hmin,g%c(3)%hmax/)
         write(un,*) 'min/max(dh)_x = ',(/g%c(1)%dhMin,g%c(1)%dhMax/)
         write(un,*) 'min/max(dh)_y = ',(/g%c(2)%dhMin,g%c(2)%dhMax/)
         write(un,*) 'min/max(dh)_z = ',(/g%c(3)%dhMin,g%c(3)%dhMax/)
         write(un,*) 'N_cells_x,stretching_x = ',g%c(1)%N,g%c(1)%dhMax-g%c(1)%dhMin
         write(un,*) 'N_cells_y,stretching_y = ',g%c(2)%N,g%c(2)%dhMax-g%c(2)%dhMin
         write(un,*) 'N_cells_z,stretching_z = ',g%c(3)%N,g%c(3)%dhMax-g%c(3)%dhMin
       end subroutine

       subroutine print_Grid(g)
         implicit none
         type(grid), intent(in) :: g
         call display(g,6)
       end subroutine

       subroutine export_grid(g,un) ! Import / Export must mirror
         implicit none
         type(grid), intent(in) :: g
         integer,intent(in) :: un
         integer :: i
         write(un,*) 'volume,defined = ';         write(un,*) g%volume,g%defined
         do i = 1,3;  call export(g%c(i),un);          enddo
       end subroutine

       subroutine import_grid(g,un) ! Import / Export must mirror
         implicit none
         type(grid), intent(inout) :: g
         integer,intent(in) :: un
         integer :: i
         read(un,*); read(un,*) g%volume,g%defined
         do i = 1,3;  call import(g%c(i),un);          enddo
         call initProps(g)
       end subroutine

       ! **********************************************************
       ! **********************************************************
       ! **********************************************************

       subroutine initProps_grid(g)
         implicit none
         type(grid),intent(inout) :: g
         integer :: i
         g%volume = g%c(1)%maxRange*g%c(2)%maxRange*g%c(3)%maxRange
         g%defined = all((/(g%c(i)%defined,i=1,3)/))
       end subroutine
        
       ! ------------------- restrict (for multigrid) --------------

       subroutine restrictGrid1(r,g,dir)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         integer,intent(in) :: dir
         call restrict(r%c(dir),g%c(dir))
       end subroutine

       subroutine restrictGrid3(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         integer :: i
         do i = 1,3; call restrict(r%c(i),g%c(i)) ;enddo
       end subroutine

       subroutine restrictGrid_x(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         call restrict(r%c(1),g%c(1))
       end subroutine

       subroutine restrictGrid_xy(r,g)
         type(grid),intent(inout) :: r
         type(grid),intent(in) :: g
         call restrict(r%c(1),g%c(1))
         call restrict(r%c(2),g%c(2))
       end subroutine

       subroutine pop_grid(g,dir) ! Removes the last index from the grid
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call pop(g%c(dir))
       end subroutine

       subroutine snip_grid(g,dir) ! Removes the first index from the grid
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call snip(g%c(dir))
       end subroutine

       ! ---------------------------------------------- check grid

#ifdef _DEBUG_COORDINATES_
       subroutine checkGrid(g)
         implicit none
         type(grid),intent(in) :: g
         integer :: i
         do i=1,3; call checkCoordinates(g%c(i)); enddo
       end subroutine
#endif

       ! *********************************************************************
       ! *********************************************************************
       ! ******************** GET SURFACE / EDGE / CORNER ********************
       ! *********************************************************************
       ! *********************************************************************

       subroutine get_face_grid_GI(g,g_in,face)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: face
         call init(g,g_in)
         if (min_face(face)) call get_GI(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_GI(g%c(dir_given_face(face)), 1)
       end subroutine

       subroutine get_face_grid_g(g,g_in,face)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: face
         call init(g,g_in)
         if (min_face(face)) call get_ghost(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_ghost(g%c(dir_given_face(face)), 1)
       end subroutine
       subroutine get_face_grid_b(g,g_in,face)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: face
         call init(g,g_in)
         if (min_face(face)) call get_boundary(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_boundary(g%c(dir_given_face(face)), 1)
       end subroutine
       subroutine get_face_grid_i(g,g_in,face)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: face
         call init(g,g_in)
         if (min_face(face)) call get_interior(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_interior(g%c(dir_given_face(face)), 1)
       end subroutine

       subroutine get_edge_grid_b(g,g_in,edge)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: edge
         type(grid) :: temp
         integer,dimension(2) :: f
         f = adj_faces_given_edge(edge)
         call get_face_b(temp,g_in,f(1))
         call get_face_b(g,temp,f(2))
       end subroutine
       subroutine get_edge_grid_g(g,g_in,edge)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: edge
         type(grid) :: temp
         integer,dimension(2) :: f
         f = adj_faces_given_edge(edge)
         call get_face_g(temp,g_in,f(1))
         call get_face_g(g,temp,f(2))
       end subroutine
       subroutine get_edge_grid_i(g,g_in,edge)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: edge
         type(grid) :: temp
         integer,dimension(2) :: f
         f = adj_faces_given_edge(edge)
         call get_face_i(temp,g_in,f(1))
         call get_face_i(g,temp,f(2))
       end subroutine

       subroutine get_corner_grid_b(g,g_in,corner)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: corner
         type(grid) :: A,B
         integer,dimension(3) :: f
         f = adj_faces_given_corner(corner)
         call get_face_b(A,g_in,f(1))
         call get_face_b(B,A,f(2))
         call get_face_b(g,B,f(3))
       end subroutine
       subroutine get_corner_grid_g(g,g_in,corner)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: corner
         type(grid) :: A,B
         integer,dimension(3) :: f
         f = adj_faces_given_corner(corner)
         call get_face_g(A,g_in,f(1))
         call get_face_g(B,A,f(2))
         call get_face_g(g,B,f(3))
       end subroutine
       subroutine get_corner_grid_i(g,g_in,corner)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: corner
         type(grid) :: A,B
         integer,dimension(3) :: f
         f = adj_faces_given_corner(corner)
         call get_face_i(A,g_in,f(1))
         call get_face_i(B,A,f(2))
         call get_face_i(g,B,f(3))
       end subroutine

       end module