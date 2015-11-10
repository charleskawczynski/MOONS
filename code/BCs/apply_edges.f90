       module apply_edges_mod
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
       !       o Data locations that are potentiall defined (depending on 
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
       !                e(i)%minmin - corresponding to {(y,z),(x,z),(x,y)}
       !                e(i)%minmax - corresponding to {(y,z),(x,z),(x,y)}
       !                e(i)%maxmin - corresponding to {(y,z),(x,z),(x,y)}
       !                e(i)%maxmax - corresponding to {(y,z),(x,z),(x,y)}
       !         for direction i, covering all 12 edge.
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
       public :: apply_edges

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       interface apply_edges;       module procedure apply_edges_VF;     end interface
       interface apply_edges;       module procedure apply_edges_SF;     end interface

       contains

       subroutine apply_edges_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_edges(U%x,m)
         call apply_edges(U%y,m)
         call apply_edges(U%z,m)
       end subroutine

       subroutine apply_edges_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_edge_SF(U,m,1,2,3,(/1,2,3,4/))
         call apply_edge_SF(U,m,2,1,3,(/5,6,7,8/))
         call apply_edge_SF(U,m,3,1,2,(/9,10,11,12/))
       end subroutine

       subroutine apply_edge_SF(U,m,dir,d1,d2,e)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,d1,d2
         integer,dimension(4),intent(in) :: e
         integer :: i
         logical :: CCd1,CCd2
         CCd1 = CC_along(U,d1)
         CCd2 = CC_along(U,d2)
         do i=1,m%s
          call app_minmin(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(1))%vals,&
          U%RF(i)%b%e(e(1))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! minmin
          call app_minmax(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(2))%vals,&
          U%RF(i)%b%e(e(2))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! minmax
          call app_maxmin(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(3))%vals,&
          U%RF(i)%b%e(e(3))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! maxmin
          call app_maxmax(U%RF(i)%f,m%g(i),U%RF(i)%b%e(e(4))%vals,&
          U%RF(i)%b%e(e(4))%b,d1,d2,U%RF(i)%s,CCd1,CCd2,dir) ! maxmax
         enddo
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
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(1)/),&
                              1,1,1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(1)/),&
                              1,1,1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
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
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              2,1,-1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              2,1,-1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
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
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(1)/),&
                              3,-1,1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(1)/),&
                              3,-1,1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
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
         case (1); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         case (2); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         case (3); call app_E(v,bct,s(d1),s(d2),CCd1,CCd2,dir,f,d1,d2,&
                              (/g%c(d1)%dhc(g%c(d1)%sc-1),g%c(d2)%dhc(g%c(d2)%sc-1)/),&
                              (/g%c(d1)%dhn(g%c(d1)%sn-1),g%c(d2)%dhn(g%c(d2)%sn-1)/),&
                              4,-1,-1)
         end select
       end subroutine

       subroutine app_E(v,bct,s1,s2,CCd1,CCd2,dir,f,d1,d2,dhc,dhn,corner,m1,m2)
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
         integer,intent(in) :: dir,s1,s2,d1,d2,corner
         integer,intent(in) :: m1,m2 ! index to move along d1 and d2 TOWARDS THE INTERIOR
         logical,intent(in) :: CCd1,CCd2
         integer :: i1,i2 ! Refence index (e.g. CC ghost cell edge boundary)

         if (CCd1.and.CCd2) then
           select case (corner)
           case (1); i1 = 1; i2 = 1
           case (2); i1 = 1; i2 = s2
           case (3); i1 = s1; i2 = 1
           case (4); i1 = s1; i2 = s2
           end select
         elseif ((.not.CCd1).and.(.not.CCd2)) then
           select case (corner)
           case (1); i1 = 2; i2 = 2
           case (2); i1 = 2; i2 = s2-1
           case (3); i1 = s1-1; i2 = 2
           case (4); i1 = s1-1; i2 = s2-1
           end select
         elseif (CCd1.and.(.not.CCd2)) then
           select case (corner)
           case (1); i1 = 1; i2 = 2
           case (2); i1 = 1; i2 = s2-1
           case (3); i1 = s1; i2 = 2
           case (4); i1 = s1; i2 = s2-1
           end select
         elseif ((.not.CCd1).and.CCd2) then
           select case (corner)
           case (1); i1 = 2; i2 = 1
           case (2); i1 = 2; i2 = s2
           case (3); i1 = s1-1; i2 = 1
           case (4); i1 = s1-1; i2 = s2
           end select
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
         else; stop 'Error: case not found in app_E in apply_edges.f90'
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
         elseif (bct%Neumann) then; ug = -(ug1*dh(1) + ug2*dh(2))/(dh(1)+dh(2)) ! (hard coded zero)
         else; stop 'Error: Bad bctype! Caught in a_CC in apply_edges.f90'
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
         elseif (bct%Neumann) then; ug1 = ui1; ug2 = ui2 ! Neumann   - N data (hard coded zero)
         else; stop 'Error: Bad bctype! Caught in a_N in apply_edges.f90'
         endif
       end subroutine

       subroutine a_F(bct,dh,bvals,ug,ui)
         !     F :    ug, ui
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: ui,bvals
         real(cp),dimension(2),intent(in) :: dh
         type(bctype),intent(in) :: bct
         if (bct%Dirichlet) then;   ug = 2.0_cp*bvals - ui
         elseif (bct%Neumann) then; ug = ui + dh*bvals
         else; stop 'Error: Bad bctype! Caught in a_F in apply_edges.f90'
         endif
       end subroutine

       end module