       module grid_extend_mod
       use grid_mod
      ! Pre-processor directives: (_DEBUG_COORDINATES_)
       use current_precision_mod
       use array_mod
       use data_location_mod
       use face_edge_corner_indexing_mod
       use IO_tools_mod
       use coordinates_mod
       use coordinates_extend_mod
       implicit none

       private
       public :: grid
       public :: init ! Essentials

       public :: restrict_x,restrict_xy ! specifically for MG

       public :: restrict
       public :: prolongate

       public :: initProps
       public :: snip,pop

       public :: mirror_about_hmin
       public :: mirror_about_hmax

       public :: get_shape

       public :: get_coordinates_h
       public :: get_coordinates_dh
       public :: get_coordinates_dual_h
       public :: get_coordinates_dual_dh

       public :: get_face_GI
       public :: get_edge_GI
       public :: get_corner_GI
       public :: get_face_b
       public :: get_edge_b
       public :: get_corner_b

#ifdef _DEBUG_COORDINATES_
       public :: checkGrid
#endif

       interface init;                     module procedure initGrid1;                  end interface
       interface init;                     module procedure initGrid2;                  end interface

       interface initProps;                module procedure initProps_grid;             end interface

       interface restrict;                 module procedure restrictGrid1;              end interface
       interface restrict;                 module procedure restrictGrid3;              end interface
       interface restrict_x;               module procedure restrictGrid_x;             end interface
       interface restrict_xy;              module procedure restrictGrid_xy;            end interface

       interface restrict;                 module procedure restrict_dir_g;             end interface
       interface restrict;                 module procedure restrict_all_g;             end interface
       interface restrict;                 module procedure restrict_dir2_g;            end interface
       interface prolongate;               module procedure prolongate_dir_g;           end interface
       interface prolongate;               module procedure prolongate_all_g;           end interface

       interface snip;                     module procedure snip_grid;                  end interface
       interface pop;                      module procedure pop_grid;                   end interface

       interface mirror_about_hmin;        module procedure mirror_about_hmin_g;        end interface
       interface mirror_about_hmax;        module procedure mirror_about_hmax_g;        end interface

       interface get_shape;                module procedure get_shape_g;                end interface

       interface get_face_GI;              module procedure get_face_GI_grid;           end interface
       interface get_edge_GI;              module procedure get_edge_GI_grid;           end interface
       interface get_corner_GI;            module procedure get_corner_GI_grid;         end interface

       interface get_face_b;               module procedure get_face_grid_b;            end interface
       interface get_face_b;               module procedure get_face_grid_b_IO;         end interface
       interface get_edge_b;               module procedure get_edge_grid_b;            end interface
       interface get_corner_b;             module procedure get_corner_grid_b;          end interface

       interface get_coordinates_h;        module procedure get_coordinates_h_g;        end interface
       interface get_coordinates_dh;       module procedure get_coordinates_dh_g;       end interface
       interface get_coordinates_dual_h;   module procedure get_coordinates_dual_h_g;   end interface
       interface get_coordinates_dual_dh;  module procedure get_coordinates_dual_dh_g;  end interface


       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine initGrid1(g,h,dir)
         implicit none
         type(grid),intent(inout) :: g
         real(cp),dimension(:),intent(in) :: h
         integer,intent(in) :: dir
         integer :: i
         call init(g%c(dir),h,size(h))
         if (all((/(allocated(g%c(i)%hn%f),i=1,3)/))) then
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

       ! ***********************************************************
       ! ******************* RESTRICT / PROLONGATE *****************
       ! ***********************************************************

       subroutine restrict_all_g(g)
         type(grid),intent(inout) :: g
         call restrict(g%c(1))
         call restrict(g%c(2))
         call restrict(g%c(3))
       end subroutine

       subroutine restrict_dir_g(g,dir)
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call restrict(g%c(dir))
       end subroutine

       subroutine restrict_dir2_g(g,dir)
         type(grid),intent(inout) :: g
         integer,dimension(2),intent(in) :: dir
         call restrict(g%c(dir(1)))
         call restrict(g%c(dir(2)))
       end subroutine

       subroutine prolongate_all_g(g)
         type(grid),intent(inout) :: g
         call prolongate(g%c(1))
         call prolongate(g%c(2))
         call prolongate(g%c(3))
       end subroutine

       subroutine prolongate_dir_g(g,dir)
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call prolongate(g%c(dir))
       end subroutine

       ! ***********************************************************
       ! ***********************************************************
       ! ***********************************************************

       subroutine pop_grid(g,dir) ! Removes the last index from the grid
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call pop(g%c(dir))
       end subroutine

       subroutine mirror_about_hmin_g(g,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call mirror_about_hmin(g%c(dir))
       end subroutine

       subroutine mirror_about_hmax_g(g,dir)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: dir
         call mirror_about_hmax(g%c(dir))
       end subroutine

       function get_shape_g(g,DL) result(s)
         implicit none
         type(grid),intent(in) :: g
         type(data_location),intent(in) :: DL
         integer,dimension(3) :: s
         integer :: i
         do i=1,3
             if ( N_along(DL,i)) then; s(i) = g%c(i)%sn
         elseif (CC_along(DL,i)) then; s(i) = g%c(i)%sc
         !     if ( N_along(DL,i)) then; s(i) = g%c(i)%h(1)%N
         ! elseif (CC_along(DL,i)) then; s(i) = g%c(i)%h(2)%N
         else; stop 'Error: bad DL in get_shape_g in grid.f90'
         endif
         enddo
       end function

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

       subroutine get_face_GI_grid(g,g_in,face)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: face
         call init(g,g_in)
         if (min_face(face)) call get_GI(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_GI(g%c(dir_given_face(face)), 1)
       end subroutine

       subroutine get_edge_GI_grid(g,g_in,edge)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: edge
         type(grid) :: temp
         integer,dimension(2) :: f
         f = adj_faces_given_edge(edge)
         call get_face_GI(temp,g_in,f(1))
         call get_face_GI(g,temp,f(2))
       end subroutine

       subroutine get_corner_GI_grid(g,g_in,corner)
         implicit none
         type(grid),intent(inout) :: g
         type(grid),intent(in) :: g_in
         integer,intent(in) :: corner
         type(grid) :: A,B
         integer,dimension(3) :: f
         f = adj_faces_given_corner(corner)
         call get_face_GI(A,g_in,f(1))
         call get_face_GI(B,A,f(2))
         call get_face_GI(g,B,f(3))
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
       subroutine get_face_grid_b_IO(g,face)
         implicit none
         type(grid),intent(inout) :: g
         integer,intent(in) :: face
         if (min_face(face)) call get_boundary(g%c(dir_given_face(face)),-1)
         if (max_face(face)) call get_boundary(g%c(dir_given_face(face)), 1)
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

      subroutine get_coordinates_h_g(h,g,DL)
        implicit none
        type(array),dimension(3),intent(inout) :: h
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3
            if ( N_along(DL,i)) then; call init(h(i),g%c(i)%hn)
        elseif (CC_along(DL,i)) then; call init(h(i),g%c(i)%hc)
        else; stop 'Error: bad DL in get_coordinates_h_g in grid.f90'
        endif
        enddo
      end subroutine

      subroutine get_coordinates_dh_g(h,g,DL)
        implicit none
        type(array),dimension(3),intent(inout) :: h
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3
            if ( N_along(DL,i)) then; call init(h(i),g%c(i)%dhn)
        elseif (CC_along(DL,i)) then; call init(h(i),g%c(i)%dhc)
        else; stop 'Error: bad DL in get_coordinates_dh_g in grid.f90'
        endif
        enddo
      end subroutine

      subroutine get_coordinates_dual_h_g(h,g,DL)
        implicit none
        type(array),dimension(3),intent(inout) :: h
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3
            if ( N_along(DL,i)) then; call init(h(i),g%c(i)%hc)
        elseif (CC_along(DL,i)) then; call init(h(i),g%c(i)%hn)
        else; stop 'Error: bad DL in get_coordinates_dual_h_g in grid.f90'
        endif
        enddo
      end subroutine

      subroutine get_coordinates_dual_dh_g(h,g,DL)
        implicit none
        type(array),dimension(3),intent(inout) :: h
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i
        do i=1,3
            if ( N_along(DL,i)) then; call init(h(i),g%c(i)%dhc)
        elseif (CC_along(DL,i)) then; call init(h(i),g%c(i)%dhn)
        else; stop 'Error: bad DL in get_coordinates_dual_dh_g in grid.f90'
        endif
        enddo
      end subroutine

      end module