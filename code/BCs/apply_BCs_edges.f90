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
       use RF_mod
       use SF_mod
       use VF_mod
       use bctype_mod
       use BCs_mod
       use grid_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs_edges

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       interface apply_BCs_edges;       module procedure apply_BCs_edges_VF;     end interface
       interface apply_BCs_edges;       module procedure apply_BCs_edges_SF;     end interface

       contains

       subroutine apply_BCs_edges_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs_edges(U%x,m)
         call apply_BCs_edges(U%y,m)
         call apply_BCs_edges(U%z,m)
       end subroutine

       subroutine apply_BCs_edges_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_edge_SF(U,m,1,2,3,(/1,2,3,4/))     ! Y-Z, right hand rule
         call apply_edge_SF(U,m,2,1,3,(/5,6,7,8/))     ! X-Z, left  hand rule
         call apply_edge_SF(U,m,3,1,2,(/9,10,11,12/))  ! X-Y, right hand rule
       end subroutine

       function direction_from_face(f) result(d)
         implicit none
         integer,intent(in) :: f
         integer :: d
         select case (f)
         case (1,2); d = 1
         case (3,4); d = 2
         case (5,6); d = 3
         case default; stop 'Error: face must = 1:6 in direction_from_face in apply_BCs_edges.f90'
         end select
       end function

       function is_min(f) result(TF)
         implicit none
         integer,intent(in) :: f
         logical :: TF
         select case (f)
         case (1,3,5); TF = .true.
         case (2,4,6); TF = .false.
         case default; stop 'Error: face must = 1:6 in is_min in apply_BCs_edges.f90'
         end select
       end function

       subroutine apply_edge_SF(U,m,dir,d1,d2,e)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,d1,d2
         integer,dimension(4),intent(in) :: e
         integer :: i
         integer,dimension(2) :: a,fd
         logical :: CCd1,CCd2
         logical,dimension(4) :: TF
         logical,dimension(2) :: TF_prep
         CCd1 = CC_along(U,d1)
         CCd2 = CC_along(U,d2)
         if (m%s.gt.1) then; do i=1,m%s
           ! Conditions to apply edges (ALL must be true): 
           ! 1) At least 1 adjacent face must be Dirichlet
           ! 2) The Dirichlet face must not be stitched
           ! 3) The other adjacent face cannot be Periodic

           a = adjacent_faces(e(1))
           fd(1) = direction_from_face(a(1)); fd(2) = direction_from_face(a(2))
           if (is_min(a(1))) then;
                 TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           else; TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           endif
           if (is_min(a(2))) then;
                 TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           else; TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           endif
           TF(1) = all(TF_prep)

           a = adjacent_faces(e(2))
           fd(1) = direction_from_face(a(1)); fd(2) = direction_from_face(a(2))
           if (is_min(a(1))) then;
                 TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           else; TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           endif
           if (is_min(a(2))) then;
                 TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           else; TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           endif
           TF(2) = all(TF_prep)

           a = adjacent_faces(e(3))
           fd(1) = direction_from_face(a(1)); fd(2) = direction_from_face(a(2))
           if (is_min(a(1))) then;
                 TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           else; TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           endif
           if (is_min(a(2))) then;
                 TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           else; TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           endif
           TF(3) = all(TF_prep)

           a = adjacent_faces(e(4))
           fd(1) = direction_from_face(a(1)); fd(2) = direction_from_face(a(2))
           if (is_min(a(1))) then;
                 TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           else; TF_prep(1) = U%RF(i)%b%f(a(1))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(1)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(1))%b%Periodic))
           endif
           if (is_min(a(2))) then;
                 TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmin(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           else; TF_prep(2) = U%RF(i)%b%f(a(2))%b%Dirichlet.and.(.not.m%g(i)%st_face%hmax(fd(2)))&
                                                           .and.(.not.(U%RF(i)%b%f(a(2))%b%Periodic))
           endif
           TF(4) = all(TF_prep)

           if (TF(1)) call app_minmin(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(1))%vals,&
            U%RF(i)%b%e(e(1))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! minmin
           if (TF(2)) call app_minmax(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(2))%vals,&
            U%RF(i)%b%e(e(2))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! minmax
           if (TF(3)) call app_maxmin(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(3))%vals,&
            U%RF(i)%b%e(e(3))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! maxmin
           if (TF(4)) call app_maxmax(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(4))%vals,&
            U%RF(i)%b%e(e(4))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! maxmax
         enddo; endif
       end subroutine

       subroutine app_minmin(f,g,v,bct,d1,d2,s,CCd1,CCd2,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,d1,d2
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(1)/),&
                              1,1,1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(1)/),&
                              1,1,1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(1)/),&
                              1,1,1)
         end select
       end subroutine

       subroutine app_minmax(f,g,v,bct,d1,d2,s,CCd1,CCd2,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,d1,d2
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              2,1,-1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              2,1,-1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              2,1,-1)
         end select
       end subroutine

       subroutine app_maxmin(f,g,v,bct,d1,d2,s,CCd1,CCd2,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,d1,d2
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(1)/),&
                              3,-1,1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(1)/),&
                              3,-1,1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(1)/),&
                              3,-1,1)
         end select
       end subroutine

       subroutine app_maxmax(f,g,v,bct,d1,d2,s,CCd1,CCd2,dir)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,d1,d2
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         end select
       end subroutine

       subroutine app_E(v,bct,s1,s2,CCd1,CCd2,dir,f,dhc,dhn,corner,m1,m2)
         ! At this point, data should be sent in a convention so that the 
         ! next set of routines can handle applying BCs in a systematic way.
         ! Below are some notes and diagrams illustrating this process:
         ! 
         !  NOTES:
         !      *: values passed in
         !      $: BC that must be enforced
         !      N: For Dirichlet, only need to define $ and linearly extrapolate after
         !         For Neumann, need many more values
         !     CC: For Dirichlet, the average of all values must equal BC value
         !         For Neumann, use weighted average, using expression from ref:
         !         Numerical Simulation in Fluid Dynamics a Practical Introduction - M. Griebel et al
         !  F(d1): Follow CC BC for face-BCs
         !  F(d2): Follow CC BC for face-BCs
         !     CC:   {1,2,3,4} = {ug,ui,ug1,ug2}
         !     N :   {1,2,3,4} = {ub,ug1,ug2,ui1,ui2}
         !     F :   {1,2}     = {ug,ui}
         ! 
         ! CC:    d2                            N :    d2                         
         !        ^                                    ^                          
         !        |       |       |                    |       |       |          
         !        |       |       |                    |       | ui2   |          
         !         ------- ------- -----                -------*------- -----     
         !        |       |       |                    |       |       |          
         !        |   *ug2|   *ui |                    |       |       |          
         !        |       |       |                    |       | ub    | ui1         
         !         -------$------- -----           ug1 *-------$-------*-----     
         !        |       |       |                    |       |       |          
         !        |   *ug |   *ug1|                    |       |       |          
         !        |       |       |                    |       |       |          
         !         ------- ------- -----> d1            -------*------- -----> d1 
         !                                                     ug2      
         ! 
         ! F(d1): d2                            F(d2): d2                         
         !        ^                                    ^                          
         !        |       |       |                    |       |       |          
         !        |       |       |                    |       |       |          
         !         ------- ------- -----                ------- ------- -----     
         !        |       |       |                    |       |       |          
         !        |       * ui    |                    |       |       |          
         !        |       |       |                    |       |       |          
         !         -------$------- -----                ---*---$---*--- -----     
         !        |       |       |                    |  ug   |   ui  |          
         !        |       * ug    |                    |       |       |          
         !        |       |       |                    |       |       |          
         !         ------- ------- -----> d1            ------- ------- -----> d1 
         ! 
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         real(cp),dimension(2),intent(in) :: dhc,dhn ! dhc,dhn for d1,d2 respectively
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,s1,s2,corner
         integer,intent(in) :: m1,m2 ! index to move along d1 and d2 TOWARDS THE INTERIOR
         logical,intent(in) :: CCd1,CCd2
         integer :: i1,i2 ! Refence index (e.g. CC ghost cell edge boundary)

         if (CCd1.and.CCd2) then
           select case (corner)
           case (1); i1 = 1; i2 = 1
           case (2); i1 = 1; i2 = s2
           case (3); i1 = s1; i2 = 1
           case (4); i1 = s1; i2 = s2
           case default; stop 'Error: corner must = 1:4 in app_E in apply_BCs_edges.f90'
           end select
         elseif ((.not.CCd1).and.(.not.CCd2)) then
           select case (corner)
           case (1); i1 = 2; i2 = 2
           case (2); i1 = 2; i2 = s2-1
           case (3); i1 = s1-1; i2 = 2
           case (4); i1 = s1-1; i2 = s2-1
           case default; stop 'Error: corner must = 1:4 in app_E in apply_BCs_edges.f90'
           end select
         elseif (CCd1.and.(.not.CCd2)) then
           select case (corner)
           case (1); i1 = 1; i2 = 2
           case (2); i1 = 1; i2 = s2-1
           case (3); i1 = s1; i2 = 2
           case (4); i1 = s1; i2 = s2-1
           case default; stop 'Error: corner must = 1:4 in app_E in apply_BCs_edges.f90'
           end select
         elseif ((.not.CCd1).and.CCd2) then
           select case (corner)
           case (1); i1 = 2; i2 = 1
           case (2); i1 = 2; i2 = s2
           case (3); i1 = s1-1; i2 = 1
           case (4); i1 = s1-1; i2 = s2
           case default; stop 'Error: corner must = 1:4 in app_E in apply_BCs_edges.f90'
           end select
         else; stop 'Error: case not found in apply_BCs_edges.f90'
         endif

         if     (CCd1.and.CCd2)               then
           select case (dir) !            ug            ui             ug1           ug2
           case (1); call a_CC(bct,dhc,v,f(:,i1,i2),f(:,i1+m1,i2+m2),f(:,i1+m1,i2),f(:,i1,i2+m2))
           case (2); call a_CC(bct,dhc,v,f(i1,:,i2),f(i1+m1,:,i2+m2),f(i1+m1,:,i2),f(i1,:,i2+m2))
           case (3); call a_CC(bct,dhc,v,f(i1,i2,:),f(i1+m1,i2+m2,:),f(i1+m1,i2,:),f(i1,i2+m2,:))
           end select
         elseif ((.not.CCd1).and.(.not.CCd2)) then
           select case (dir) !            ub          ug1           ug2           ui1           ui2
           case (1); call a_N(bct,dhn,v,f(:,i1,i2),f(:,i1-m1,i2),f(:,i1,i2-m2),f(:,i1+m1,i2),f(:,i1,i2+m2))
           case (2); call a_N(bct,dhn,v,f(i1,:,i2),f(i1-m1,:,i2),f(i1,:,i2-m2),f(i1+m1,:,i2),f(i1,:,i2+m2))
           case (3); call a_N(bct,dhn,v,f(i1,i2,:),f(i1-m1,i2,:),f(i1,i2-m2,:),f(i1+m1,i2,:),f(i1,i2+m2,:))
           end select
         elseif ((.not.CCd1).and.(CCd2))      then ! F(d1)
           select case (dir) ! minmin     ug        ui
           case (1); call a_F(bct,dhc(2),v,f(:,i1,i2),f(:,i1,i2+m2))
           case (2); call a_F(bct,dhc(2),v,f(i1,:,i2),f(i1,:,i2+m2))
           case (3); call a_F(bct,dhc(2),v,f(i1,i2,:),f(i1,i2+m2,:))
           end select
         elseif (CCd1.and.(.not.CCd2))        then ! F(d2)
           select case (dir) ! minmin     ug        ui
           case (1); call a_F(bct,dhc(1),v,f(:,i1,i2),f(:,i1+m1,i2))
           case (2); call a_F(bct,dhc(1),v,f(i1,:,i2),f(i1+m1,:,i2))
           case (3); call a_F(bct,dhc(1),v,f(i1,i2,:),f(i1+m1,i2,:))
           end select
         else; stop 'Error: case not found in app_E in apply_BCs_edges.f90'
         endif
       end subroutine

       subroutine debug_app_E(v,bct,s1,s2,CCd1,CCd2,dir,f,dhc,dhn,corner,m1,m2,i1,i2,name)
         implicit none
         real(cp),dimension(:,:,:),intent(in) :: f
         real(cp),dimension(2),intent(in) :: dhc,dhn
         real(cp),dimension(:),intent(in) :: v
         type(bctype),intent(in) :: bct
         integer,intent(in) :: dir,s1,s2,corner
         integer,intent(in) :: m1,m2,i1,i2
         logical,intent(in) :: CCd1,CCd2
         character(len=*),intent(in) :: name
         write(*,*) ' -------------------- ',name,' -------------------- '
         write(*,*) 'maxval(f) = ',maxval(f)
         if (maxval(abs(f)).gt.2.0_cp) then
           write(*,*) 'maxval(f) = ',maxval(f)
           write(*,*) 'dhc,dhn = ',dhc,dhn
           write(*,*) 'CCd1,CCd2 = ',CCd1,CCd2
           write(*,*) 'dir,corner = ',dir,corner
           write(*,*) 'i1,i2,m1,m2 = ',i1,i2,m1,m2
           write(*,*) 's1,s2 = ',s1,s2
           write(*,*) 'v = ',v
           select case (dir)
           case (1); write(*,*) 'f(:,i1,i2),f(:,i1,i2+m2) = ',f(:,i1,i2),f(:,i1,i2+m2)
           case (2); write(*,*) 'f(i1,:,i2),f(i1,:,i2+m2) = ',f(i1,:,i2),f(i1,:,i2+m2)
           case (3); write(*,*) 'f(i1,i2,:),f(i1,i2+m2,:) = ',f(i1,i2,:),f(i1,i2+m2,:)
           end select
           stop 'Bad edge values in app_E in apply_BCs_edges.f90'
         endif
       end subroutine

       subroutine a_CC(bct,dh,bvals,ug,ui,ug1,ug2)
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

       subroutine a_N(bct,dh,bvals,ub,ug1,ug2,ui1,ui2)
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

       subroutine a_F(bct,dh,bvals,ug,ui)
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