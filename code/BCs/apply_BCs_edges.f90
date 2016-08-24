       module apply_BCs_edges_mod
       ! Notes:
       !       o Edge BCs ARE NOT USED FOR PERIODIC BCs. It was decided that
       !         periodic BCs are typically used for simple geometries, not to
       !         mention they would only be applicable across 1 grid (and not 1 mesh)
       !       o Edge BCs defines BCs at a grid's edge. This is necessary
       !         because edge BCs will not be defined if two adjoining 
       !         face-stitching occurs. 
       !       o For example, velocty is enforced 
       !         at an edge in the below diagram, where the top right block 
       !         skips face BCs on the left and bottom sides
       ! 
       !          --------------
       !                       |
       !          --------     |
       !                /|     |
       !               / |     |
       !              /  |     |
       !             /
       !           Here
       ! 
       !       o Data locations that are potentially defined (depending on 
       !         Dirichlet/Nuemann BCs) are illustrated below with asterisks
       ! 
       !        |       |       |     
       !        |       |       |     
       !         ------- ------- -----
       !        |       |       |     
       !        |       |       |     
       !        |       |       |     
       !        *---*---*------- -----
       !        |       |       |     
       !        |   *   *       |     
       !        |       |       |     
       !         -------*------- -----
       ! 
       !       o Edge data is separated into 3 (edge) directions. This is 
       !         listed and illustrated below
       !         Edges are organized as follows
       !                minmin(i)
       !                minmax(i)
       !                maxmin(i)
       !                maxmax(i)
       !         for direction i, covering all 12 edge.
       !         
       !         To be more explicit:
       !         
       !         x:  (i=1)   minmin(1):  ymin,zmin ! Right hand rule
       !                     minmax(2):  ymin,zmax ! Right hand rule
       !                     maxmin(3):  ymax,zmin ! Right hand rule
       !                     maxmax(4):  ymax,zmax ! Right hand rule
       !         
       !         y:  (i=2)   minmin(5):  xmin,zmin ! LEFT hand rule
       !                     minmax(6):  xmin,zmax ! LEFT hand rule
       !                     maxmin(7):  xmax,zmin ! LEFT hand rule
       !                     maxmax(8):  xmax,zmax ! LEFT hand rule
       !         
       !         z:  (i=3)   minmin(9):  xmin,ymin ! Right hand rule
       !                     minmax(10): xmin,ymax ! Right hand rule
       !                     maxmin(11): xmax,ymin ! Right hand rule
       !                     maxmax(12): xmax,ymax ! Right hand rule
       ! 
       !          d2
       !          ^
       !          |
       !          -------------------
       ! minmax   |   |         |   | maxmax
       !          |---|---------|---|
       !          |   |         |   |
       !          |   |         |   |
       !          |   |         |   |
       !          |---|---------|---|
       ! minmin   |   |         |   | maxmin
       !          ---------------------->d1
       ! 
       ! 
       use current_precision_mod
       use coordinates_mod
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       use face_edge_corner_indexing_mod
       implicit none

       private
       public :: apply_BCs_edges




       interface apply_BCs_edges;       module procedure apply_BCs_edges_VF;     end interface
       interface apply_BCs_edges;       module procedure apply_BCs_edges_SF;     end interface

       contains

       subroutine apply_BCs_edges_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_edges_SF(U%x,m)
         call apply_BCs_edges_SF(U%y,m)
         call apply_BCs_edges_SF(U%z,m)
       end subroutine

       subroutine apply_BCs_edges_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         if (m%s.gt.1) call apply_edge_SF(U,m)
       end subroutine

       function BC_TF(RF,g,f,e) result(L)
         !
         !        z                          x                          y                        
         !        ^    6                     ^    2                     ^    4                   
         !        2---------4                2---------4                2---------4              
         !        |         |                |         |                |         |              
         !      3 |  dir=1  | 4            5 |  dir=2  | 6            1 |  dir=3  | 2            
         !        |         |                |         |                |         |              
         !        1---------3-> y            1---------3-> z            1---------3-> x          
         !             5                          1                          3                   
         implicit none
         type(realField),intent(inout) :: RF
         type(grid),intent(in) :: g
         integer,dimension(2),intent(in) :: f
         integer,intent(in) :: e
         logical,dimension(2) :: L1,L2
         logical :: L
         L1(1) = RF%b%f(f(1))%b%Dirichlet.and.(.not.g%st_faces(f(1))%TF).and.(.not.RF%b%f(f(2))%b%periodic)
         L1(2) = RF%b%f(f(2))%b%Dirichlet.and.(.not.g%st_faces(f(2))%TF).and.(.not.RF%b%f(f(1))%b%periodic)
         L2(1) = g%st_faces(f(1))%TF.and.g%st_faces(f(2))%TF
         L2(2) = .not.g%st_edges(e)%TF
         L = any(L1).or.all(L2)
       end function

       subroutine apply_edge_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,dimension(4) :: e
         integer,dimension(2) :: a,f
         integer :: i,k
         if (U%is_CC) then
           do i=1,m%s; do k = 1,3
           e = edges_given_dir(k); a = adj_dir_given_dir(k)
           f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_CC(U%RF(i),m%g(i),e(1),a(1),a(2),k,1)
           f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_CC(U%RF(i),m%g(i),e(2),a(1),a(2),k,2)
           f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_CC(U%RF(i),m%g(i),e(3),a(1),a(2),k,3)
           f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_CC(U%RF(i),m%g(i),e(4),a(1),a(2),k,4)
           enddo; enddo
         elseif (U%is_Node) then
           do i=1,m%s; do k = 1,3
           e = edges_given_dir(k); a = adj_dir_given_dir(k)
           f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_N(U%RF(i),m%g(i),e(1),a(1),a(2),k,1)
           f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_N(U%RF(i),m%g(i),e(2),a(1),a(2),k,2)
           f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_N(U%RF(i),m%g(i),e(3),a(1),a(2),k,3)
           f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_N(U%RF(i),m%g(i),e(4),a(1),a(2),k,4)
           enddo; enddo
         elseif (U%is_Face) then
           do i=1,m%s; do k = 1,3
           e = edges_given_dir(k); a = adj_dir_given_dir(k)
           if (U%face.eq.k) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_CC(U%RF(i),m%g(i),e(1),a(1),a(2),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_CC(U%RF(i),m%g(i),e(2),a(1),a(2),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_CC(U%RF(i),m%g(i),e(3),a(1),a(2),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_CC(U%RF(i),m%g(i),e(4),a(1),a(2),k,4)
           elseif (U%face.eq.a(1)) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_F1(U%RF(i),m%g(i),e(1),a(2),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_F1(U%RF(i),m%g(i),e(2),a(2),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_F1(U%RF(i),m%g(i),e(3),a(2),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_F1(U%RF(i),m%g(i),e(4),a(2),k,4)
           elseif (U%face.eq.a(2)) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_F2(U%RF(i),m%g(i),e(1),a(1),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_F2(U%RF(i),m%g(i),e(2),a(1),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_F2(U%RF(i),m%g(i),e(3),a(1),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_F2(U%RF(i),m%g(i),e(4),a(1),k,4)
           else; stop 'Error: unhandled exception (1) in apply_BCs_edges.f90'
           endif
           enddo; enddo
         elseif (U%is_Edge) then
           do i=1,m%s; do k = 1,3
           e = edges_given_dir(k); a = adj_dir_given_dir(k)
           if (U%edge.eq.k) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_N(U%RF(i),m%g(i),e(1),a(1),a(2),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_N(U%RF(i),m%g(i),e(2),a(1),a(2),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_N(U%RF(i),m%g(i),e(3),a(1),a(2),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_N(U%RF(i),m%g(i),e(4),a(1),a(2),k,4)
           elseif (U%edge.eq.a(1)) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_F2(U%RF(i),m%g(i),e(1),a(1),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_F2(U%RF(i),m%g(i),e(2),a(1),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_F2(U%RF(i),m%g(i),e(3),a(1),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_F2(U%RF(i),m%g(i),e(4),a(1),k,4)
           elseif (U%edge.eq.a(2)) then
             f = adj_faces_given_edge(e(1)); if (BC_TF(U%RF(i),m%g(i),f,e(1))) call a_F1(U%RF(i),m%g(i),e(1),a(2),k,1)
             f = adj_faces_given_edge(e(2)); if (BC_TF(U%RF(i),m%g(i),f,e(2))) call a_F1(U%RF(i),m%g(i),e(2),a(2),k,2)
             f = adj_faces_given_edge(e(3)); if (BC_TF(U%RF(i),m%g(i),f,e(3))) call a_F1(U%RF(i),m%g(i),e(3),a(2),k,3)
             f = adj_faces_given_edge(e(4)); if (BC_TF(U%RF(i),m%g(i),f,e(4))) call a_F1(U%RF(i),m%g(i),e(4),a(2),k,4)
           else; stop 'Error: unhandled exception (2) in apply_BCs_edges.f90'
           endif
           enddo; enddo
         else; stop 'Error: bad data input to apply_edges_SF in apply_BCs_edge.f90'
         endif
       end subroutine

       subroutine a_CC(RF,g,e,d1,d2,dir,corner)
         implicit none
         type(realField),intent(inout) :: RF
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,d1,d2,corner,e
#ifdef _DEBUG_APPLY_BCS_
         if (RF%s(d1).ne.g%c(d1)%sc) stop 'Error: bad input to a_CC (1) in apply_BCs_edge.f90'
         if (RF%s(d2).ne.g%c(d2)%sc) stop 'Error: bad input to a_CC (2) in apply_BCs_edge.f90'
#endif
         call app_CC_RF(RF%f,RF%b%e(e)%vals,RF%b%e(e)%b,RF%s(1),RF%s(2),RF%s(3),dir,g%c(d1),g%c(d2),corner)
       end subroutine

       subroutine a_N(RF,g,e,d1,d2,dir,corner)
         implicit none
         type(realField),intent(inout) :: RF
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,d1,d2,corner,e
#ifdef _DEBUG_APPLY_BCS_
         if (RF%s(d1).ne.g%c(d1)%sn) stop 'Error: bad input to a_N (1) in apply_BCs_edge.f90'
         if (RF%s(d2).ne.g%c(d2)%sn) stop 'Error: bad input to a_N (2) in apply_BCs_edge.f90'
#endif
         call app_N_RF(RF%f,RF%b%e(e)%vals,RF%b%e(e)%b,RF%s(1),RF%s(2),RF%s(3),dir,g%c(d1),g%c(d2),corner)
       end subroutine

       subroutine a_F1(RF,g,e,d2,dir,corner)
         implicit none
         type(realField),intent(inout) :: RF
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,d2,corner,e
#ifdef _DEBUG_APPLY_BCS_
         if (RF%s(d2).ne.g%c(d2)%sc) stop 'Error: bad input to a_F1 (2) in apply_BCs_edge.f90'
#endif
         call app_F1_RF(RF%f,RF%b%e(e)%vals,RF%b%e(e)%b,RF%s(1),RF%s(2),RF%s(3),dir,g%c(d2),corner)
       end subroutine

       subroutine a_F2(RF,g,e,d1,dir,corner)
         implicit none
         type(realField),intent(inout) :: RF
         type(grid),intent(in) :: g
         integer,intent(in) :: dir,d1,corner,e
#ifdef _DEBUG_APPLY_BCS_
         if (RF%s(d1).ne.g%c(d1)%sc) stop 'Error: bad input to a_F2 (1) in apply_BCs_edge.f90'
#endif
         call app_F2_RF(RF%f,RF%b%e(e)%vals,RF%b%e(e)%b,RF%s(1),RF%s(2),RF%s(3),dir,g%c(d1),corner)
       end subroutine

       subroutine app_CC_RF(f,v,bct,x,y,z,dir,c1,c2,corner)
         ! At this point, data should be sent in a convention so that the 
         ! next set of routines can handle applying BCs in a systematic way.
         ! Below are some notes and diagrams illustrating this process:
         ! 
         !  NOTES:
         !      $: BC that must be enforced
         !     CC: For Dirichlet, the average of all values must equal BC value
         !         For Neumann, use weighted average, using expression from ref:
         !         Numerical Simulation in Fluid Dynamics a Practical Introduction - M. Griebel et al
         ! 
         !        d2                                                   
         !        ^                                                    
         !        |                                                    
         !        |                                                    
         !         ------- ------- -------- -------- -------          
         !        |       |       |        |        |       |         
         !   2    |  *ug  |  *ug1 |        |  *ug1  |  *ug  |    4    
         !        |       |       |        |        |       |         
         !         -------$------- -------- --------$-------          
         !        |       |       |        |        |       |         
         !        |  *ug2 |  *ui  |        |  *ui   |  *ug2 |         
         !        |       |       |        |        |       |         
         !         ------- ------- -------- -------- -------          
         !        |       |       |        |        |       |         
         !        |       |       |        |        |       |         
         !        |       |       |        |        |       |         
         !         ------- ------- -------- -------- -------          
         !        |       |       |        |        |       |         
         !        |  *ug2 |  *ui  |        |  *ui   |  *ug2 |         
         !        |       |       |        |        |       |         
         !   1     -------$------- -------- --------$-------     3    
         !        |       |       |        |        |       |         
         !        |  *ug  |  *ug1 |        |  *ug1  |  *ug  |         
         !        |       |       |        |        |       |         
         !         ------- ------- -------- -------- ------- -----> d1
         ! 
         !        z                      x                      y                   
         !        ^                      ^                      ^                   
         !        |-----                 |-----                 |-----              
         !        |     |     dir = 1    |     |     dir = 2    |     |     dir = 3
         !        |     |                |     |                |     |             
         !         -------> y             -------> z             -------> x         
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         type(coordinates),intent(in) :: c1,c2
         integer,intent(in) :: dir,corner
         integer,intent(in) :: x,y,z ! s(1),s(2),s(3)
         select case (corner)
         case (1)
         select case (dir) !                           ug         ui      ug1       ug2
         case (1); call app_CC(bct,(/c1%dhc(1),c2%dhc(1)/),v,f(:,1,1),f(:,2,2),f(:,2,1),f(:,1,2))
         case (2); call app_CC(bct,(/c1%dhc(1),c2%dhc(1)/),v,f(1,:,1),f(2,:,2),f(1,:,2),f(2,:,1))
         case (3); call app_CC(bct,(/c1%dhc(1),c2%dhc(1)/),v,f(1,1,:),f(2,2,:),f(2,1,:),f(1,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (2)
         select case (dir) !                           ug         ui        ug1        ug2
         case (1); call app_CC(bct,(/c1%dhc(1),c2%dhc_e/),v,f(:,1,z),f(:,2,z-1),f(:,2,z),f(:,1,z-1))
         case (2); call app_CC(bct,(/c1%dhc(1),c2%dhc_e/),v,f(x,:,1),f(x-1,:,2),f(x,:,2),f(x-1,:,1))
         case (3); call app_CC(bct,(/c1%dhc(1),c2%dhc_e/),v,f(1,y,:),f(2,y-1,:),f(2,y,:),f(1,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (3)
         select case (dir) !                           ug         ui        ug1        ug2
         case (1); call app_CC(bct,(/c1%dhc_e,c2%dhc(1)/),v,f(:,y,1),f(:,y-1,2),f(:,y-1,1),f(:,y,2))
         case (2); call app_CC(bct,(/c1%dhc_e,c2%dhc(1)/),v,f(1,:,z),f(2,:,z-1),f(1,:,z-1),f(2,:,z))
         case (3); call app_CC(bct,(/c1%dhc_e,c2%dhc(1)/),v,f(x,1,:),f(x-1,2,:),f(x-1,1,:),f(x,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (4)
         select case (dir) !                           ug         ui           ug1        ug2
         case (1); call app_CC(bct,(/c1%dhc_e,c2%dhc_e/),v,f(:,y,z),f(:,y-1,z-1),f(:,y-1,z),f(:,y,z-1))
         case (2); call app_CC(bct,(/c1%dhc_e,c2%dhc_e/),v,f(x,:,z),f(x-1,:,z-1),f(x,:,z-1),f(x-1,:,z))
         case (3); call app_CC(bct,(/c1%dhc_e,c2%dhc_e/),v,f(x,y,:),f(x-1,y-1,:),f(x-1,y,:),f(x,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case default; stop 'Error: corner must = 1,2,3,4 in app_E_SF in apply_BCs_edges.f90'
         end select
       end subroutine

       subroutine app_N_RF(f,v,bct,x,y,z,dir,c1,c2,corner)
         ! At this point, data should be sent in a convention so that the 
         ! next set of routines can handle applying BCs in a systematic way.
         ! Below are some notes and diagrams illustrating this process:
         ! 
         !  NOTES:
         !      $: BC that must be enforced
         !      N: For Dirichlet, the average of all values must equal BC value
         !         For Neumann, use weighted average, using expression from ref:
         !         Numerical Simulation in Fluid Dynamics a Practical Introduction - M. Griebel et al
         ! 
         !   N :    d2                                                        
         !          ^                                                         
         !                  ug2                             ug2                   
         !           -------*------- ------ ------- --------*---------            
         !          |       |       |      |       |        |         |           
         !   2      |       |       |      |       |        |         |          4
         !          |       | ub    | ui1  |       |  ui1   | ub      |           
         !      ug1 *-------$-------*------*-------*--------$---------* ug1       
         !          |       |       |      |       |        |         |           
         !          |       |       |      |       |        |         |           
         !          |       | ui2   |      |       |        | ui2     |           
         !           -------*------- ------ ------- --------*---------            
         !          |       |       |      |       |        |         |           
         !          |       |       |      |       |        |         |           
         !           -------*------- ------ ------- --------*---------            
         !          |       |       |      |       |        |         |           
         !          |       | ui2   |      |       |        | ui2     |           
         !           -------*------- ------ ------- --------*---------            
         !          |       |       |      |       |        |         |           
         !   1      |       |       |      |       |        |         |          3
         !          |       | ub    | ui1  |       |  ui1   | ub      |           
         !      ug1 *-------$-------*------*-------*--------$---------* ug1       
         !          |       |       |      |       |        |         |           
         !          |       |       |      |       |        |         |           
         !          |       |       |      |       |        |         |           
         !           -------*------- ------ ------- --------*---------     > d1   
         !                  ug2                             ug2               
         !        z                      x                      y                   
         !        ^                      ^                      ^                   
         !        |-----                 |-----                 |-----              
         !        |     |     dir = 1    |     |     dir = 2    |     |     dir = 3
         !        |     |                |     |                |     |             
         !         -------> y             -------> z             -------> x         
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         type(coordinates),intent(in) :: c1,c2
         integer,intent(in) :: dir,corner
         integer,intent(in) :: x,y,z ! s(1),s(2),s(3)
         select case (corner)
         case (1)
         select case (dir) !                           ub       ug1        ug2      ui1      ui2
         case (1); call app_N(bct,(/c1%dhn(1),c2%dhn(1)/),v,f(:,2,2),f(:,1,2),f(:,2,1),f(:,3,2),f(:,2,3))
         case (2); call app_N(bct,(/c1%dhn(1),c2%dhn(1)/),v,f(2,:,2),f(2,:,1),f(1,:,2),f(2,:,3),f(3,:,2))
         case (3); call app_N(bct,(/c1%dhn(1),c2%dhn(1)/),v,f(2,2,:),f(1,2,:),f(2,1,:),f(3,2,:),f(2,3,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (2)
         select case (dir) !                            ub        ug1       ug2         ui1         ui2
         case (1); call app_N(bct,(/c1%dhn(1),c2%dhn_e/),v,f(:,2,z-1),f(:,1,z-1),f(:,2,z),f(:,3,z-1),f(:,2,z-2))
         case (2); call app_N(bct,(/c1%dhn(1),c2%dhn_e/),v,f(x-1,:,2),f(x-1,:,1),f(x,:,2),f(x-1,:,3),f(x-2,:,2))
         case (3); call app_N(bct,(/c1%dhn(1),c2%dhn_e/),v,f(2,y-1,:),f(1,y-1,:),f(2,y,:),f(3,y-1,:),f(2,y-2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (3)
         select case (dir) !                           ub         ug1        ug2        ui1        ui2
         case (1); call app_N(bct,(/c1%dhn_e,c2%dhn(1)/),v,f(:,y-1,2),f(:,y,2),f(:,y-1,1),f(:,y-2,2),f(:,y-1,3))
         case (2); call app_N(bct,(/c1%dhn_e,c2%dhn(1)/),v,f(2,:,z-1),f(2,:,z),f(1,:,z-1),f(2,:,z-2),f(3,:,z-1))
         case (3); call app_N(bct,(/c1%dhn_e,c2%dhn(1)/),v,f(x-1,2,:),f(x,2,:),f(x-1,1,:),f(x-2,2,:),f(x-1,3,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (4)
         select case (dir) !                           ub       ug1        ug2         ui1         ui2
         case (1); call app_N(bct,(/c1%dhn_e,c2%dhn_e/),v,f(:,y-1,z-1),f(:,y,z-1),f(:,y-1,z),f(:,y-2,z-1),f(:,y-1,z-2))
         case (2); call app_N(bct,(/c1%dhn_e,c2%dhn_e/),v,f(x-1,:,z-1),f(x-1,:,z),f(x,:,z-1),f(x-1,:,z-2),f(x-2,:,z-1))
         case (3); call app_N(bct,(/c1%dhn_e,c2%dhn_e/),v,f(x-1,y-1,:),f(x,y-1,:),f(x-1,y,:),f(x-2,y-1,:),f(x-1,y-2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case default; stop 'Error: corner must = 1,2,3,4 in app_E_SF in apply_BCs_edges.f90'
         end select
       end subroutine

       subroutine app_F1_RF(f,v,bct,x,y,z,dir,c2,corner)
         ! At this point, data should be sent in a convention so that the 
         ! next set of routines can handle applying BCs in a systematic way.
         ! Below are some notes and diagrams illustrating this process:
         ! 
         !  NOTES:
         !      $: BC that must be enforced
         !  F(d1): For Dirichlet, the average of all values must equal BC value
         !         For Neumann, use weighted average, using expression from ref:
         !         Numerical Simulation in Fluid Dynamics a Practical Introduction - M. Griebel et al
         ! 
         !        d2                                                
         !        ^                                                 
         !        |                                                 
         !        |                                                 
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !   2    |       | ug    |        |     ug |       |       4
         !        |       |       |        |        |       |       
         !         -------$------- -------- --------$-------        
         !        |       |       |        |        |       |       
         !        |       | ui    |        |     ui |       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !        |       * ui    |        |     ui *       |       
         !        |       |       |        |        |       |       
         !   1     -------$-------$-------- --------$-------        3
         !        |       |       |        |        |       |       
         !        |       * ug    |        |     ug *       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- ------- -----> d1
         !
         !        z                      x                      y                   
         !        ^                      ^                      ^                   
         !        |-----                 |-----                 |-----              
         !        |     |     dir = 1    |     |     dir = 2    |     |     dir = 3
         !        |     |                |     |                |     |             
         !         -------> y             -------> z             -------> x         
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         type(coordinates),intent(in) :: c2
         integer,intent(in) :: dir,corner
         integer,intent(in) :: x,y,z ! s(1),s(2),s(3)
         select case (corner)
         case (1)
         select case (dir) !                            ug        ui
         case (1); call app_F(bct,c2%dhc(1),v,f(:,2,1),f(:,2,2))
         case (2); call app_F(bct,c2%dhc(1),v,f(1,:,2),f(2,:,2))
         case (3); call app_F(bct,c2%dhc(1),v,f(2,1,:),f(2,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (2)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c2%dhc_e,v,f(:,2,z),f(:,2,z-1))
         case (2); call app_F(bct,c2%dhc_e,v,f(x,:,2),f(x-1,:,2))
         case (3); call app_F(bct,c2%dhc_e,v,f(2,y,:),f(2,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (3)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c2%dhc(1),v,f(:,y-1,1),f(:,y-1,2))
         case (2); call app_F(bct,c2%dhc(1),v,f(1,:,z-1),f(2,:,z-1))
         case (3); call app_F(bct,c2%dhc(1),v,f(x-1,1,:),f(x-1,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (4)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c2%dhc_e,v,f(:,y-1,z),f(:,y-1,z-1))
         case (2); call app_F(bct,c2%dhc_e,v,f(x,:,z-1),f(x-1,:,z-1))
         case (3); call app_F(bct,c2%dhc_e,v,f(x-1,y,:),f(x-1,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case default; stop 'Error: corner must = 1,2,3,4 in app_E_SF in apply_BCs_edges.f90'
         end select
       end subroutine

       subroutine app_F2_RF(f,v,bct,x,y,z,dir,c1,corner)
         ! At this point, data should be sent in a convention so that the 
         ! next set of routines can handle applying BCs in a systematic way.
         ! Below are some notes and diagrams illustrating this process:
         ! 
         !  NOTES:
         !      $: BC that must be enforced
         !  F(d2): For Dirichlet, the average of all values must equal BC value
         !         For Neumann, use weighted average, using expression from ref:
         !         Numerical Simulation in Fluid Dynamics a Practical Introduction - M. Griebel et al
         ! 
         !        d2                                                
         !        ^                                                 
         !        |                                                 
         !        |                                                 
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !   2     ---*---$--*----$------- -----*---$--*-----       4
         !        |  ug   |  ui   |        |   ui   |  ug   |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- -------        
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !   1     ---*---$--*---- -------- ----*---$--*----        3
         !        |  ug   |  ui   |        |   ui   |  ug   |       
         !        |       |       |        |        |       |       
         !        |       |       |        |        |       |       
         !         ------- ------- -------- -------- ------- -----> d1
         !
         !        z                      x                      y                   
         !        ^                      ^                      ^                   
         !        |-----                 |-----                 |-----              
         !        |     |     dir = 1    |     |     dir = 2    |     |     dir = 3
         !        |     |                |     |                |     |             
         !         -------> y             -------> z             -------> x         
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         type(coordinates),intent(in) :: c1
         integer,intent(in) :: dir,corner
         integer,intent(in) :: x,y,z ! s(1),s(2),s(3)
         select case (corner)
         case (1)
         select case (dir) !                            ug        ui
         case (1); call app_F(bct,c1%dhc(1),v,f(:,1,2),f(:,2,2))
         case (2); call app_F(bct,c1%dhc(1),v,f(2,:,1),f(2,:,2))
         case (3); call app_F(bct,c1%dhc(1),v,f(1,2,:),f(2,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (2)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c1%dhc(1),v,f(:,1,z-1),f(:,2,z-1))
         case (2); call app_F(bct,c1%dhc(1),v,f(x-1,:,1),f(x-1,:,2))
         case (3); call app_F(bct,c1%dhc(1),v,f(1,y-1,:),f(2,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (3)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c1%dhc_e,v,f(:,y,2),f(:,y-1,2))
         case (2); call app_F(bct,c1%dhc_e,v,f(2,:,z),f(2,:,z-1))
         case (3); call app_F(bct,c1%dhc_e,v,f(x,2,:),f(x-1,2,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case (4)
         select case (dir) !                           ug        ui
         case (1); call app_F(bct,c1%dhc_e,v,f(:,y,z-1),f(:,y-1,z-1))
         case (2); call app_F(bct,c1%dhc_e,v,f(x-1,:,z),f(x-1,:,z-1))
         case (3); call app_F(bct,c1%dhc_e,v,f(x,y-1,:),f(x-1,y-1,:))
         case default; stop 'Error: dir must = 1,2,3 in app_E_SF in apply_BCs_edges.f90'
         end select
         case default; stop 'Error: corner must = 1,2,3,4 in app_E_SF in apply_BCs_edges.f90'
         end select
       end subroutine

       subroutine app_CC(bct,dh,bvals,ug,ui,ug1,ug2)
         !     CC:    ug, ui  , ug1 , ug2
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: bvals,ui,ug1,ug2
         real(cp),dimension(2),intent(in) :: dh
         type(bctype),intent(in) :: bct
         if (bct%Dirichlet) then;   ug = 4.0_cp*bvals - (ui + ug1 + ug2)
         elseif (bct%Neumann) then; 
         ug = (ug1*dh(1) + ug2*dh(2))/(dh(1)+dh(2)) ! (hard coded zero)
         else; stop 'Error: Bad bctype! Caught in a_CC in apply_BCs_edges.f90'
         endif
       end subroutine

       subroutine app_N(bct,dh,bvals,ub,ug1,ug2,ui1,ui2)
         !     N :    ub, ug1 , ug2 , ui1 , ui2
         implicit none
         real(cp),dimension(:),intent(inout) :: ub,ug1,ug2
         real(cp),dimension(:),intent(in) :: bvals,ui1,ui2
         real(cp),dimension(2),intent(in) :: dh ! needed for non-zero Neumann
         type(bctype),intent(in) :: bct
         if (bct%Dirichlet) then;   ub = bvals
         elseif (bct%Neumann) then; 
           ug1 = ui1 - 2.0_cp*dh(1)*bvals
           ug2 = ui2 - 2.0_cp*dh(2)*bvals
         else; stop 'Error: Bad bctype! Caught in a_N in apply_BCs_edges.f90'
         endif
       end subroutine

       subroutine app_F(bct,dh,bvals,ug,ui)
         !     F :    ug, ui
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: ui,bvals
         real(cp),intent(in) :: dh
         type(bctype),intent(in) :: bct
         if (bct%Dirichlet) then;   ug = 2.0_cp*bvals - ui
         elseif (bct%Neumann) then; ug = ui + dh*bvals
         else; stop 'Error: Bad bctype! Caught in a_F in apply_BCs_edges.f90'
         endif
       end subroutine


       end module