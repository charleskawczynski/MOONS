       module face_edge_corner_indexing_mod
       ! This module provides routines to obtain indexes for various things.
       ! The figure at the bottom of this document illustrates the convention used.
       use current_precision_mod
       implicit none

       private
       public :: eye_given_dir

       public :: dir_given_face
       public :: dir_given_edge
       public :: adj_dir_given_dir
       public :: adj_dir_given_face
       public :: adj_dir_given_edge
       public :: orth_dir

       public :: adj_shape_given_dir

       public :: adj_faces_given_edge
       public :: adj_faces_given_corner
       public :: normal_faces_given_dir
       public :: adj_faces_given_dir
       public :: opp_face_given_face
       public :: min_face
       public :: max_face

       public :: edges_given_dir
       public :: edges_given_face

       public :: corners_given_face

       public :: nhat_given_face
       public :: nhat_given_edge
       public :: xyz_given_dir

       public :: insist_valid_dir,   valid_dir
       public :: insist_valid_face,  valid_face
       public :: insist_valid_edge,  valid_edge
       public :: insist_valid_corner,valid_corner

       contains

       ! *************************************************************************
       ! ******************************** GET EYE ********************************
       ! *************************************************************************

       function eye_given_dir(dir) result(eye)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(3) :: eye
         select case (dir)
         case (1); eye = (/1,0,0/)
         case (2); eye = (/0,1,0/)
         case (3); eye = (/0,0,1/)
         case default; stop 'Error: dir must = 1:3 in eye_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       ! *************************************************************************
       ! ******************************** GET DIR ********************************
       ! *************************************************************************

       function dir_given_face(face) result(dir)
         implicit none
         integer,intent(in) :: face
         integer :: dir
         select case (face)
         case (1,2); dir = 1
         case (3,4); dir = 2
         case (5,6); dir = 3
         case default; stop 'Error: face must = 1:6 in dir_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       function dir_given_edge(edge) result(dir)
         implicit none
         integer,intent(in) :: edge
         integer :: dir
         select case (edge)
         case (1:4);  dir = 1
         case (5:8);  dir = 2
         case (9:12); dir = 3
         case default; stop 'Error: face must = 1:12 in dir_given_edge in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_dir_given_dir(dir) result (a)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(2) :: a
         select case (dir)
         case (1); a = (/2,3/)
         case (2); a = (/1,3/)
         case (3); a = (/1,2/)
         case default; stop 'Error: dir must = 1,2,3 in adj_dir_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_shape_given_dir(s,dir) result (a)
         implicit none
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir
         integer,dimension(2) :: a
         select case (dir)
         case (1); a = (/s(2),s(3)/)
         case (2); a = (/s(1),s(3)/)
         case (3); a = (/s(1),s(2)/)
         case default; stop 'Error: dir must = 1,2,3 in adj_shape_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_dir_given_face(face) result (a)
         implicit none
         integer,intent(in) :: face
         integer,dimension(2) :: a
         select case (face)
         case (1,2); a = (/2,3/)
         case (3,4); a = (/3,1/)
         case (5,6); a = (/1,2/)
         case default; stop 'Error: face must = 1:6 in adj_dir_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_dir_given_edge(edge) result (dir)
         implicit none
         integer,intent(in) :: edge
         integer,dimension(2) :: dir
         select case (edge)
         case (1:4);  dir = (/2,3/)
         case (5:8);  dir = (/1,3/)
         case (9:12); dir = (/1,2/)
         case default; stop 'Error: edge must = 1:12 in adj_dir_given_edge in face_edge_corner_indexing.f90'
         end select
       end function

       function orth_dir(dir) result(d)
         implicit none
         integer,dimension(2),intent(in) :: dir
         integer :: d
         select case (dir(1))
         case (1); select case (dir(2))
                   case (2); d = 3
                   case (3); d = 2
                   case default; stop 'Error: dir(1)=1, but dir(2)!=2,3 in orth_dir in face_edge_corner_indexing.f90'
                   end select
         case (2); select case (dir(2))
                   case (1); d = 3
                   case (3); d = 1
                   case default; stop 'Error: dir(1)=2, but dir(2)!=1,3 in orth_dir in face_edge_corner_indexing.f90'
                   end select
         case (3); select case (dir(2))
                   case (1); d = 2
                   case (2); d = 1
                   case default; stop 'Error: dir(1)=3, but dir(2)!=1,2 in orth_dir in face_edge_corner_indexing.f90'
                   end select
         case default; stop 'Error: dir(1)!=1,2,3 in orth_dir in face_edge_corner_indexing.f90'
         end select
       end function

       ! *************************************************************************
       ! ******************************* GET FACE ********************************
       ! *************************************************************************

       function adj_faces_given_edge(edge) result (faces)
         implicit none
         integer,intent(in) :: edge
         integer,dimension(2) :: faces
         select case (edge)
         case (1);  faces = (/3,5/) ! x (ymin,zmin) (x)
         case (2);  faces = (/3,6/) ! x (ymin,zmax) (x)
         case (3);  faces = (/4,5/) ! x (ymax,zmin) (x)
         case (4);  faces = (/4,6/) ! x (ymax,zmax) (x)
         case (5);  faces = (/5,1/) ! y (zmin,xmin) (y)
         case (6);  faces = (/5,2/) ! y (zmin,xmax) (y)
         case (7);  faces = (/6,1/) ! y (zmax,xmin) (y)
         case (8);  faces = (/6,2/) ! y (zmax,xmax) (y)
         case (9);  faces = (/1,3/) ! z (xmin,ymin) (z)
         case (10); faces = (/1,4/) ! z (xmin,ymax) (z)
         case (11); faces = (/2,3/) ! z (xmax,ymin) (z)
         case (12); faces = (/2,4/) ! z (xmax,ymax) (z)
         case default; stop 'Error: edge must = 1:12 in adj_faces_given_edge in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_faces_given_corner(corner) result(faces)
        implicit none
        integer,intent(in) :: corner
        integer,dimension(3) :: faces
        select case (corner)
        case (1); faces = (/1,3,5/)
        case (2); faces = (/2,3,5/)
        case (3); faces = (/1,4,5/)
        case (4); faces = (/1,3,6/)
        case (5); faces = (/1,4,6/)
        case (6); faces = (/2,3,6/)
        case (7); faces = (/2,4,5/)
        case (8); faces = (/2,4,6/)
        case default; stop 'Error: bad case in adj_faces_given_corner in face_edge_corner_indexing.f90'
        end select
       end function

       function normal_faces_given_dir(dir) result (faces)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(2) :: faces
         select case (dir)
         case (1); faces = (/1,2/)
         case (2); faces = (/3,4/)
         case (3); faces = (/5,6/)
         case default; stop 'Error: dir must = 1,2,3 in normal_faces_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_faces_given_dir(dir) result (faces)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(4) :: faces
         select case (dir)
         case (1); faces = (/3,4,5,6/)
         case (2); faces = (/1,2,5,6/)
         case (3); faces = (/1,2,3,4/)
         case default; stop 'Error: dir must = 1,2,3 in adj_faces_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function opp_face_given_face(face) result (f_opp)
         implicit none
         integer,intent(in) :: face
         integer :: f_opp
         select case (face)
         case(1); f_opp = 2
         case(2); f_opp = 1
         case(3); f_opp = 4
         case(4); f_opp = 3
         case(5); f_opp = 6
         case(6); f_opp = 5
         case default; stop 'Error: bad case in opp_face_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       function min_face(face) result(L)
        implicit none
        integer,intent(in) :: face
        logical :: L
        select case (face)
        case(1,3,5); L = .true.
        case(2,4,6); L = .false.
        case default; stop 'Error: bad case in min_face in face_edge_corner_indexing.f90'
        end select
       end function

       function max_face(face) result(L)
        implicit none
        integer,intent(in) :: face
        logical :: L
        select case (face)
        case(1,3,5); L = .false.
        case(2,4,6); L = .true.
        case default; stop 'Error: bad case in max_face in face_edge_corner_indexing.f90'
        end select
       end function

       ! *************************************************************************
       ! ******************************* GET EDGES *******************************
       ! *************************************************************************

       function edges_given_dir(dir) result (edges)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(4) :: edges
         select case (dir)
         case (1); edges = (/1,2,3,4/)
         case (2); edges = (/5,6,7,8/)
         case (3); edges = (/9,10,11,12/)
         case default; stop 'Error: dir must = 1,2,3 in edges_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function edges_given_face(face) result (edges)
         implicit none
         integer,intent(in) :: face
         integer,dimension(4) :: edges
         select case (face)
         case (1,2); edges = (/1,2,3,4/)
         case (3,4); edges = (/5,6,7,8/)
         case (5,6); edges = (/9,10,11,12/)
         case default; stop 'Error: face must = 1,2,3 in edges_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       ! *************************************************************************
       ! ****************************** GET CORNERS ******************************
       ! *************************************************************************

       function corners_given_face(face) result (corners)
         implicit none
         integer,intent(in) :: face
         integer,dimension(4) :: corners
         select case (face)
         case (1); corners = (/1,4,3,5/)
         case (2); corners = (/2,6,7,8/)
         case (3); corners = (/1,2,4,6/)
         case (4); corners = (/3,7,5,8/)
         case (5); corners = (/1,3,2,7/)
         case (6); corners = (/4,5,6,8/)
         case default; stop 'Error: dir must = 1,2,3 in corners_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       ! *************************************************************************
       ! ********************************* OTHER *********************************
       ! *************************************************************************

       function nhat_given_face(face) result (nhat)
         implicit none
         integer,intent(in) :: face
         real(cp) :: nhat
         select case (face)
         case(1); nhat = -1.0_cp
         case(2); nhat =  1.0_cp
         case(3); nhat = -1.0_cp
         case(4); nhat =  1.0_cp
         case(5); nhat = -1.0_cp
         case(6); nhat =  1.0_cp
         case default; stop 'Error: bad case in nhat_given_face in face_edge_corner_indexing.f90'
         end select
       end function

       function nhat_given_edge(edge) result (nhat)
         implicit none
         integer,intent(in) :: edge
         real(cp),dimension(2) :: nhat
         integer,dimension(2) :: faces
         faces = adj_faces_given_edge(edge)
         nhat = (/nhat_given_face(faces(1)),nhat_given_face(faces(2))/)
       end function

       function xyz_given_dir(dir) result(c)
         implicit none
         integer,intent(in) :: dir
         character(len=1) :: c
         select case (dir)
         case (1); c = 'x'
         case (2); c = 'y'
         case (3); c = 'z'
         case default; stop 'Error: dir must = 1:3 in face_edge_corner_indexing.f90'
         end select
       end function

       ! *************************************************************************
       ! ********************************* VALID *********************************
       ! *************************************************************************

       subroutine insist_valid_dir(dir,caller)
         implicit none
         integer,intent(in) :: dir
         character(len=*),intent(in) :: caller
         if (.not.valid_dir(dir)) then
         write(*,*) 'Error: invalid dir in ',caller,' in face_edge_corner_indexing.f90'
         write(*,*) 'dir = ',dir
         stop 'DONE'
         endif
       end subroutine

       subroutine insist_valid_face(face,caller)
         implicit none
         integer,intent(in) :: face
         character(len=*),intent(in) :: caller
         if (.not.valid_face(face)) then
         write(*,*) 'Error: invalid face in ',caller,' in face_edge_corner_indexing.f90'
         write(*,*) 'face = ',face
         stop 'DONE'
         endif
       end subroutine

       subroutine insist_valid_edge(edge,caller)
         implicit none
         integer,intent(in) :: edge
         character(len=*),intent(in) :: caller
         if (.not.valid_edge(edge)) then
         write(*,*) 'Error: invalid edge in ',caller,' in face_edge_corner_indexing.f90'
         write(*,*) 'edge = ',edge
         stop 'DONE'
         endif
       end subroutine

       subroutine insist_valid_corner(corner,caller)
         implicit none
         integer,intent(in) :: corner
         character(len=*),intent(in) :: caller
         if (.not.valid_corner(corner)) then
         write(*,*) 'Error: invalid corner in ',caller,' in face_edge_corner_indexing.f90'
         write(*,*) 'corner = ',corner
         stop 'DONE'
         endif
       end subroutine

       function valid_dir(dir) result(L)
         implicit none
         integer,intent(in) :: dir
         logical :: L
         select case (dir)
         case (1:3); L = .true.
         case default; L = .false.
         end select
       end function

       function valid_face(face) result(L)
         implicit none
         integer,intent(in) :: face
         logical :: L
         select case (face)
         case (1:6); L = .true.
         case default; L = .false.
         end select
       end function

       function valid_edge(edge) result(L)
         implicit none
         integer,intent(in) :: edge
         logical :: L
         select case (edge)
         case (1:12); L = .true.
         case default; L = .false.
         end select
       end function

       function valid_corner(corner) result(L)
         implicit none
         integer,intent(in) :: corner
         logical :: L
         select case (corner)
         case (1:8); L = .true.
         case default; L = .false.
         end select
       end function

         ! This module provides routines to obtain indexes for BC
         ! and stitch data structures so that consistent indexing
         ! is used. A figure below illustrates the convention used.
         !
         ! ************************************* DIRECTION *************************************
         ! SLICE PATTERN:
         ! Direction 1: x
         ! Direction 2: y
         ! Direction 3: z
         !
         ! ID NUMBERS:
         !            2
         !            y
         !            ^
         !            |
         !            |
         !            ------------------------
         !           /|                     /|
         !          / |                    / |
         !         /  |                   /  |
         !        ------------------------   |
         !        |   |                  |   |
         !        |   |                  |   |
         !        |   |                  |   |
         !        |   |------------------|---|---->x 1
         !        |  /                   |  /
         !        | /                    | /
         !        |/                     |/
         !        ------------------------
         !       /
         !      /
         !     z
         !    3
         !
         ! ************************************* FACES *************************************
         ! SLICE PATTERN:
         ! Face 1: X(1,:,:)
         ! Face 2: X(N,:,:)
         ! Face 3: X(:,1,:)
         ! Face 4: X(:,N,:)
         ! Face 5: X(:,:,1)
         ! Face 6: X(:,:,N)
         !
         ! ID NUMBERS:
         !            y
         !            ^
         !            |
         !            |
         !            ------------------------
         !           /|                     /|
         !          / |       4            / |
         !         /  |                   /  |
         !        ------------------------   |
         !        |   |          5       |   |
         !        | 1 |                  | 2 |
         !        |   |       6          |   |
         !        |   |------------------|---|---->x
         !        |  /                   |  /
         !        | /         3          | /
         !        |/                     |/
         !        ------------------------
         !       /
         !      /
         !     z
         !
         ! ************************************ EDGES ************************************
         ! SLICE PATTERN:
         ! Edge 1:  X(:,1,1)
         ! Edge 2:  X(:,1,N)
         ! Edge 3:  X(:,N,1)
         ! Edge 4:  X(:,N,N)
         ! Edge 5:  X(1,:,1)
         ! Edge 6:  X(N,:,1)
         ! Edge 7:  X(1,:,N)
         ! Edge 8:  X(N,:,N)
         ! Edge 9:  X(1,1,:)
         ! Edge 10: X(1,N,:)
         ! Edge 11: X(N,1,:)
         ! Edge 12: X(N,N,:)
         !
         ! ID NUMBERS:
         !            y
         !            ^
         !            |
         !            |
         !            ----------3-------------
         !           /|                     /|
         !         10 |                   12 |
         !         /  |                   /  |
         !        ------------4-----------   |
         !        |   5                  |   6
         !        |   |                  |   |
         !        |   |                  |   |
         !        7   |                  8   |
         !        |   |----------1-------|---|---->x
         !        |  /                   |  /
         !        | 9                    | 11
         !        |/                     |/
         !        -----------2------------
         !       /
         !      /
         !     z
         !
         ! ALTERNATIVE, 2D VIEW
         !        z                          x                          y
         !        ^    6                     ^    2                     ^    4
         !        2---------4                2---------4                2---------4
         !        |         |                |         |                |         |
         !      3 |  dir=1  | 4            5 |  dir=2  | 6            1 |  dir=3  | 2
         !        |         |                |         |                |         |
         !        1---------3-> y            1---------3-> z            1---------3-> x
         !             5                          1                          3
         !
         !          e_pad=0                    e_pad=4                     e_pad=8
         !
         ! OR, IN GENERAL,
         !
         !        d2
         !        ^
         !        2---------4
         !        |         |
         !        |   dir   |
         !        |         |
         !        1---------3-> d1
         !
         ! ************************************ CORNERS ************************************
         ! SLICE PATTERN:
         ! Corner 1: X(1,1,1)
         ! Corner 2: X(N,1,1) ! N eye amongst 1's
         ! Corner 3: X(1,N,1) ! N eye amongst 1's
         ! Corner 4: X(1,1,N) ! N eye amongst 1's
         ! Corner 5: X(1,N,N) ! 1 eye amongst N's
         ! Corner 6: X(N,1,N) ! 1 eye amongst N's
         ! Corner 7: X(N,N,1) ! 1 eye amongst N's
         ! Corner 8: X(N,N,N)
         !
         ! ID NUMBERS:
         !            y
         !            ^
         !            |
         !            |
         !            3----------------------7
         !           /|                     /|
         !          / |                    / |
         !         /  |                   /  |
         !        5----------------------8   |
         !        |   |                  |   |
         !        |   |                  |   |
         !        |   |                  |   |
         !        |   1------------------|---2---->x
         !        |  /                   |  /
         !        | /                    | /
         !        |/                     |/
         !        4----------------------6
         !       /
         !      /
         !     z
         !
         ! *********************************************************************************
       end module