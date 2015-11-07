       module applyEdges_mod
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
         integer :: k
         call apply_edge_SF(U,m,1,2,3)
         call apply_edge_SF(U,m,2,1,3)
         call apply_edge_SF(U,m,3,1,2)
       end subroutine

       subroutine apply_edge_SF(U,m,dir,e,d1,d2)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir,d1,d2
         integer :: i
         logical :: CCd1,CCd2
         CCd1 = CC_along(U,d1)
         CCd2 = CC_along(U,d2)
         do i=1,m%s
          call app_minmin(U%RF(i)%f,m%g(i),U%RF(i)%b%edge(dir)%minmin%vals,U%RF(i)%b%edge(dir)%minmin%bctype,U%RF(i)%s,dir,d1,d2,CCd1,CCd2) ! minmin
          call app_minmax(U%RF(i)%f,m%g(i),U%RF(i)%b%edge(dir)%minmax%vals,U%RF(i)%b%edge(dir)%minmax%bctype,U%RF(i)%s,dir,d1,d2,CCd1,CCd2) ! minmax
          call app_maxmin(U%RF(i)%f,m%g(i),U%RF(i)%b%edge(dir)%maxmin%vals,U%RF(i)%b%edge(dir)%maxmin%bctype,U%RF(i)%s,dir,d1,d2,CCd1,CCd2) ! maxmin
          call app_maxmax(U%RF(i)%f,m%g(i),U%RF(i)%b%edge(dir)%maxmax%vals,U%RF(i)%b%edge(dir)%maxmax%bctype,U%RF(i)%s,dir,d1,d2,CCd1,CCd2) ! maxmax
         enddo
       end subroutine

       subroutine app_minmin(f,g,d1,d2,s,CCd1,CCd2,dir,pad)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,d1,d2,t,pad
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,t,s(d1),s(d2),g%c(d1)%dhc(1),g%c(d2)%dhc(1),g%c(d1)%dhn(1),g%c(d2)%dhn(1),CCd1,CCd2,dir,f,1,1,1)
         case (2); call app_E(v,t,s(d1),s(d2),g%c(d1)%dhc(1),g%c(d2)%dhc(1),g%c(d1)%dhn(1),g%c(d2)%dhn(1),CCd1,CCd2,dir,f,1,1,1)
         case (3); call app_E(v,t,s(d1),s(d2),g%c(d1)%dhc(1),g%c(d2)%dhc(1),g%c(d1)%dhn(1),g%c(d2)%dhn(1),CCd1,CCd2,dir,f,1,1,1)
         end select
       end subroutine

       subroutine app_minmax(f,g,d1,d2,s,CCd1,CCd2,dir,pad)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,d1,d2,t,pad
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,2,1,-1)
         case (2); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,2,1,-1)
         case (3); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,2,1,-1)
         end select
       end subroutine

       subroutine app_maxmin(f,g,d1,d2,s,CCd1,CCd2,dir,pad)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,d1,d2,t,pad
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,3,-1,1)
         case (2); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,3,-1,1)
         case (3); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,3,-1,1)
         end select
       end subroutine

       subroutine app_maxmax(f,g,d1,d2,s,CCd1,CCd2,dir,pad)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: f
         type(grid),intent(in) :: g
         real(cp),dimension(:),intent(in) :: v
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: dir,d1,d2,t,pad
         logical,intent(in) :: CCd1,CCd2
         select case (dir)
         case (1); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,4,-1,-1)
         case (2); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,4,-1,-1)
         case (3); call app_E(v,t,s(d1),s(d2),g%c(d1),g%c(d2),CCd1,CCd2,dir,f,4,-1,-1)
         end select
       end subroutine

       subroutine app_E(v,t,s1,s2,d1,d2,dhc,dhn,CCd1,CCd2,dir,f,corner,m1,m2)
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
         integer,intent(in) :: dir,t,s1,s2,d1,d2,corner
         integer,intent(in) :: m1,m2 ! index to move along d1 and d2 TOWARDS THE INTERIOR
         logical,intent(in) :: CCd1,CCd2
         integer,dimension(2) :: i ! Refence index (e.g. CC ghost cell edge boundary)

         if (CCd1.and.((corner.eq.1).or.(corner.eq.2))) then; i(1) = 1;  else; i(1) = 2;    endif
         if (CCd1.and.((corner.eq.3).or.(corner.eq.4))) then; i(1) = s1; else; i(1) = s1-1; endif

         if (CCd2.and.((corner.eq.1).or.(corner.eq.2))) then; i(2) = 1;  else; i(2) = 2;    endif
         if (CCd2.and.((corner.eq.3).or.(corner.eq.4))) then; i(2) = s2; else; i(2) = s2-1; endif

         if     (CCd1.and.CCd2)               then
           select case (dir) !            ug            ui             ug1           ug2
           case (1); call a_CC(t,dhc,v,f(:,i1,i2),f(:,i1+m1,i2+m2),f(:,i1+m1,i2),f(:,i1,i2+m2))
           case (2); call a_CC(t,dhc,v,f(i1,:,i2),f(i1+m1,:,i2+m2),f(i1+m1,:,i2),f(i1,:,i2+m2))
           case (3); call a_CC(t,dhc,v,f(i1,i2,:),f(i1+m1,i2+m2,:),f(i1+m1,i2,:),f(i1,i2+m2,:))
           end select
         elseif ((.not.CCd1).and.(.not.CCd2)) then
           select case (dir) !            ub          ug1           ug2           ui1           ui2
           case (1); call a_N(t,dhn,v,f(:,i1,i2),f(:,i1-m1,i2),f(:,i1,i2-m2),f(:,i1+m1,i2),f(:,i1,i2+m2))
           case (2); call a_N(t,dhn,v,f(i1,:,i2),f(i1-m1,:,i2),f(i1,:,i2-m2),f(i1+m1,:,i2),f(i1,:,i2+m2))
           case (3); call a_N(t,dhn,v,f(i1,i2,:),f(i1-m1,i2,:),f(i1,i2-m2,:),f(i1+m1,i2,:),f(i1,i2+m2,:))
           end select
         elseif ((.not.CCd1).and.(CCd2))      then ! F(d1)
           select case (dir) ! minmin     ug        ui
           case (1); call a_F(t,dhc(d2),v,f(:,i1,i2),f(:,i1,i2+m2))
           case (2); call a_F(t,dhc(d2),v,f(i1,:,i2),f(i1,:,i2+m2))
           case (3); call a_F(t,dhc(d2),v,f(i1,i2,:),f(i1,i2+m2,:))
           end select
         elseif (CCd1.and.(.not.CCd2))        then ! F(d2)
           select case (dir) ! minmin     ug        ui
           case (1); call a_F(t,dhc(d1),v,f(:,i1,i2),f(:,i1+m1,i2))
           case (2); call a_F(t,dhc(d1),v,f(i1,:,i2),f(i1+m1,:,i2))
           case (3); call a_F(t,dhc(d1),v,f(i1,i2,:),f(i1+m1,i2,:))
           end select
         else; stop 'Error: case not found in app_E in apply_edges.f90'
         endif
       end subroutine

       subroutine a_CC(bctype,dh,bvals,ug,ui,ug1,ug2)
         !     CC:    ug, ui  , ug1 , ug2
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: bvals,ui,ug1,ug2
         real(cp),dimension(2),intent(in) :: dh
         integer,intent(in) :: t
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ug = 4.0_cp*bvals - (ui + ug1 + ug2)         ! Dirichlet - CC data
         ! *************************** NEUMANN *****************************
         case (2); ug = -(ug1*dh(1) + ug2*dh(2))/(dh(1)+dh(2))  ! Neumann   - CC data (hard coded zero)
         case default
         stop 'Error: Bad bctype! Caught in apply_edges.f90'
         end select
       end subroutine

       subroutine a_N(bctype,dh,bvals,ub,ug1,ug2,ui1,ui2)
         !     N :    ub, ug1 , ug2 , ui1 , ui2
         implicit none
         real(cp),dimension(:),intent(inout) :: ub,ug1,ug2
         real(cp),dimension(:),intent(in) :: bvals,ui1,ui2
         real(cp),dimension(2),intent(in) :: dh
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ub = bvals               ! Dirichlet - N data
         ! *************************** NEUMANN *****************************
         case (2); ug1 = ui1; ug2 = ui2     ! Neumann   - N data (hard coded zero)
         case default
         stop 'Error: Bad bctype! Caught in apply_edges.f90'
         end select
       end subroutine

       subroutine app_F(bctype,dh,bvals,ug,ui)
         !     F :    ug, ui
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: ui,bvals
         real(cp),dimension(2),intent(in) :: dh
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ub = bvals; ug = 2.0_cp*bvals - ui ! Dirichlet - direct - wall coincident
         ! *************************** NEUMANN *****************************
         case (3); ub = ui; ug = ui                   ! Explicit Neumann - direct - wall coincident ~O(dh)?
         case (4); ug = ui - 2.0_cp*bvals*dh          ! Implicit Neumann - direct - wall coincident ~O(dh^2)
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       end module