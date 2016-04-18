       module face_edge_corner_indexing_mod
         ! This module provides routines to obtain indexes for BC
         ! and stitch data structures so that consistent indexing
         ! is used. A figure below illustrates the convention used.
         ! 
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
         ! Or, in general,
         ! 
         !        d2               
         !        ^                
         !        2---------4      
         !        |         |      
         !        |   dir   |      
         !        |         |      
         !        1---------3-> d1 
         !                                   
         ! For corners, we have
         ! 
         !            y                                   
         !            ^                                   
         !            |                                   
         !            |                                   
         !            5----------------------4            
         !           /|                     /|            
         !          / |                    / |            
         !         /  |                   /  |            
         !        8----------------------2   |            
         !        |   |                  |   |            
         !        |   |                  |   |            
         !        |   |                  |   |            
         !        |   1------------------|---7---->x      
         !        |  /                   |  /             
         !        | /                    | /              
         !        |/                     |/               
         !        3----------------------6                
         !       /                                        
         !      /                                         
         !     z                                          
         ! 
       use current_precision_mod
       implicit none

       private
       public :: adj_faces_given_edge
       public :: edges_given_dir
       public :: normal_faces_given_dir
       public :: adj_faces_given_dir
       public :: adj_dir_given_dir
       public :: dir_given_face

       contains

       function dir_given_face(face) result(dir)
         implicit none
         integer,intent(in) :: face
         integer :: dir
         select case (face)
         case (1,2); dir = 1
         case (3,4); dir = 2
         case (5,6); dir = 3
         case default; stop 'Error: face must = 1:6 in get_dir_given_face in apply_BCs_faces.f90'
         end select
       end function

       function adj_faces_given_edge(edge) result (faces)
         implicit none
         integer,intent(in) :: edge
         integer,dimension(2) :: faces
         select case (edge)
         case (1);  faces = (/3,5/) ! x (ymin,zmin) (x)
         case (2);  faces = (/3,6/) ! x (ymin,zmax) (x)
         case (3);  faces = (/4,5/) ! x (ymax,zmin) (x)
         case (4);  faces = (/4,6/) ! x (ymax,zmax) (x)
         case (5);  faces = (/5,1/) ! y (xmin,zmin) (y)
         case (6);  faces = (/5,2/) ! y (xmin,zmax) (y)
         case (7);  faces = (/6,1/) ! y (xmax,zmin) (y)
         case (8);  faces = (/6,2/) ! y (xmax,zmax) (y)
         case (9);  faces = (/1,3/) ! z (xmin,ymin) (z)
         case (10); faces = (/1,4/) ! z (xmin,ymax) (z)
         case (11); faces = (/2,3/) ! z (xmax,ymin) (z)
         case (12); faces = (/2,4/) ! z (xmax,ymax) (z)
         case default; stop 'Error: edge must = 1:12 in adj_faces_given_edge in face_edge_corner_indexing.f90'
         end select
       end function

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

       function adj_dir_given_dir(dir) result (a)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(2) :: a
         select case (dir)
         case (1); a = (/2,3/)
         case (2); a = (/3,1/)
         case (3); a = (/1,2/)
         case default; stop 'Error: dir must = 1,2,3 in adj_dir_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function normal_faces_given_dir(dir) result (f)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(2) :: f
         select case (dir)
         case (1); f = (/1,2/)
         case (2); f = (/3,4/)
         case (3); f = (/5,6/)
         case default; stop 'Error: dir must = 1,2,3 in normal_faces_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       function adj_faces_given_dir(dir) result (f)
         implicit none
         integer,intent(in) :: dir
         integer,dimension(4) :: f
         select case (dir)
         case (1); f = (/3,4,5,6/)
         case (2); f = (/1,2,5,6/)
         case (3); f = (/1,2,3,4/)
         case default; stop 'Error: dir must = 1,2,3 in normal_faces_given_dir in face_edge_corner_indexing.f90'
         end select
       end function

       end module