       module applyEdges_mod
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
         call apply_edge_SF(U,m,1,(/1,2,3,4/))
         call apply_edge_SF(U,m,2,(/5,6,7,8/))
         call apply_edge_SF(U,m,3,(/9,10,11,12/))
       end subroutine

       subroutine apply_edge_SF(U,m,dir,e)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer,intent(in) :: dir
         integer,dimension(4),intent(in) :: e
         integer :: i,k,d1,d2
         select case (dir)
         case (1); d1 = 2; d2 = 3
         case (2); d1 = 1; d2 = 3
         case (3); d1 = 1; d2 = 2
         case default; stop 'Error: dir must = 1,2,3 in apply_edge_SF in apply_edges.f90'
         end select
         ! The following code acts on edges in two edge-orthogonal directions:
         ! 
         !   d2
         !   ^
         !   |
         !   -------------------
         !   | 2 |         | 4 |
         !   |---|---------|---|
         !   |   |         |   |
         !   |   |         |   |
         !   |   |         |   |
         !   |---|---------|---|
         !   | 1 |         | 3 |
         !   ---------------------->d1
         ! 
         ! The following lines need to be prepended to the calls
         ! 
         ! if (.not.m%g(i)%st_edge%minmin(dir)) 
         ! if (.not.m%g(i)%st_edge%minmax(dir)) 
         ! if (.not.m%g(i)%st_edge%maxmin(dir)) 
         ! if (.not.m%g(i)%st_edge%maxmax(dir)) 

         if (CC_along(U,d1).and.CC_along(U,d2)) then         ! Can safely call CC-based BC
           do i=1,m%s; do k=1,4
           call e_CC( U%RF(i)%f ,&
                      U%RF(i)%b%edge(e(k))%vals , &
                      U%RF(i)%b%edge(e(k))%bctype , &
                      U%RF(i)%s , &
                      U%RF(i)%b%edge(e(k))%d , &
                      m%g(i) , &
                      e(k))
           enddo; enddo
         elseif (Node_along(U,d1).and.Node_along(U,d2)) then ! Can safely call node-based BC
           do i=1,m%s; do k=1,4
           call e_N ( U%RF(i)%f ,&
                      U%RF(i)%b%edge(e(k))%vals , &
                      U%RF(i)%b%edge(e(k))%bctype , &
                      U%RF(i)%s , &
                      U%RF(i)%b%edge(e(k))%d , &
                      m%g(i) , &
                      e(k))
           enddo; enddo
         elseif (Node_along(U,d1).and.CC_along(U,d2)) then   ! Must check directionto call BC
           do i=1,m%s; do k=1,4
           if (U%RF(i)%b%edge(e(k))%d.eq.d1) then
           call e_N ( U%RF(i)%f ,&
                      U%RF(i)%b%edge(e(k))%vals , &
                      U%RF(i)%b%edge(e(k))%bctype , &
                      U%RF(i)%s , &
                      U%RF(i)%b%edge(e(k))%d , &
                      m%g(i) , &
                      e(k))
           elseif (U%RF(i)%b%edge(e(k))%d.eq.d2) then
           call e_CC( U%RF(i)%f ,&
                      U%RF(i)%b%edge(e(k))%vals , &
                      U%RF(i)%b%edge(e(k))%bctype , &
                      U%RF(i)%s , &
                      U%RF(i)%b%edge(e(k))%d , &
                      m%g(i) , &
                      e(k))
           else; stop 'Error: edge type not found in apply_edges.f90'
           enddo; enddo
         else; stop 'Error: datatype not found in apply_edges.f90'
         endif
       end subroutine

       subroutine e_CC(u,g,v,t,s,e,d,dh)
         implicit none
         type(realField),intent(inout) :: u
         real(cp),dimension(:),intent(in) :: v
         type(grid),intent(in) :: g
         integer,intent(in) :: t
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: e,d ! edge, direction
         integer,dimension(3) :: i ! corner index
         integer :: d1,d2 ! d1 and d2 directions
         integer :: idh ! index for dh

         ! For readability, the faces are traversed in the order:
         !       {1,2,3,4,5,6} = (x_min,x_max,y_min,y_max,z_min,z_max)
         ! 
         ! applyBCs_Node(ug,ub,ui,ui_opp,bvals,bctype,dh)
         !
         ! 
         ! Passing arguments in the following order
         ! 
         ! Passing arguments in the following order
         !           1) Edge ghost
         !           2) Edge interior
         !           3) Edge ghost + d1
         !           4) Edge ghost + d2
         ! 
         ! Below is an illustration:
         ! 
         !          d2
         !          ^
         !          |
         !          -------------------
         ! minmax   | 1 | 3     3 | 1 | maxmax
         !          |---|---------|---|
         !          | 4 | 2     2 | 4 |
         !          |   |         |   |
         !          | 4 | 2     2 | 4 |
         !          |---|---------|---|
         ! minmin   | 1 | 3     3 | 1 | maxmin
         !          ---------------------->d1
         ! 
         select case (e) ! edge
         case (1);  i = (/1   ,1   /); d1 =  1; d2 =  1; idh = 1 ! minmin
         case (2);  i = (/1   ,s(3)/); d1 =  1; d2 = -1; if (d.eq.d1) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! minmax
         case (3);  i = (/s(2),1   /); d1 = -1; d2 =  1; if (d.eq.d2) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! maxmin
         case (4);  i = (/s(2),s(3)/); d1 = -1; d2 = -1; idh = g%c(d)%sc-1 ! maxmax

         case (5);  i = (/1   ,1   /); d1 =  1; d2 =  1; idh = g%c(d)%sc-1 ! minmin
         case (6);  i = (/1   ,s(3)/); d1 =  1; d2 = -1; if (d.eq.d1) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! minmax
         case (7);  i = (/s(1),1   /); d1 = -1; d2 =  1; if (d.eq.d2) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! maxmin
         case (8);  i = (/s(1),s(3)/); d1 = -1; d2 = -1; idh = g%c(d)%sc-1 ! maxmax

         case (9);  i = (/1   ,1   /); d1 =  1; d2 =  1; idh = g%c(d)%sc-1 ! minmin
         case (10); i = (/1   ,s(2)/); d1 =  1; d2 = -1; if (d.eq.d1) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! minmax
         case (11); i = (/s(1),1   /); d1 = -1; d2 =  1; if (d.eq.d2) then; idh = 1; else; idh = g%c(d)%sc-1; endif ! maxmin
         case (12); i = (/s(1),s(2)/); d1 = -1; d2 = -1; idh = g%c(d)%sc-1 ! maxmax
         end select

         select case (e) ! edge
         case (1:4)
         call app_CC(f(:,i(1)   ,i(2)   ),&
                     f(:,i(1)+d1,i(2)+d2),&
                     f(:,i(1)+d1,i(2)   ),&
                     f(:,i(1)   ,i(2)+d2),&
                     v,t,dh)
         case (5:8)
         call app_CC(f(i(1)   ,:,i(2)   ),&
                     f(i(1)+d1,:,i(2)+d2),&
                     f(i(1)+d1,:,i(2)   ),&
                     f(i(1)   ,:,i(2)+d2),&
                     v,t,dh)
         case (9:12)
         call app_CC(f(i(1)   ,i(2)   ,:),&
                     f(i(1)+d1,i(2)+d2,:),&
                     f(i(1)+d1,i(2)   ,:),&
                     f(i(1)   ,i(2)+d2,:),&
                     v,t,dh)
         end select

       end subroutine


       subroutine e_CC(u,g,v,t,s,e,d)
         implicit none
         real(cp),dimension(3),intent(inout) :: f
         real(cp),dimension(:),intent(in) :: v
         type(grid),intent(in) :: g
         integer,intent(in) :: bctype
         integer,dimension(3),intent(in) :: s
         integer,intent(in) :: e,d ! edge, direction
         integer,dimension(3) :: i ! corner index

         ! For readability, the faces are traversed in the order:
         !       {1,2,3,4,5,6} = (x_min,x_max,y_min,y_max,z_min,z_max)
         ! 
         ! applyBCs_Node(ug,ub,ui,ui_opp,bvals,bctype,dh)
         !
         ! 
         ! Passing arguments in the following order
         ! 
         !          d2
         !          ^
         !          |
         !          -------------------
         ! minmax   | 1 | 3     3 | 1 | maxmax
         !          |---|---------|---|
         !          | 4 | 2     2 | 4 |
         !          |   |         |   |
         !          | 4 | 2     2 | 4 |
         !          |---|---------|---|
         ! minmin   | 1 | 3     3 | 1 | maxmin
         !          ---------------------->d1
         ! 
         select case (e) ! edge
         case (1);  call app_CC(f(:,1,1),f(:,2,2),f(:,2,1),f(:,1,2),v,t,dh,dho) ! x: minmin
         case (5);  call app_CC(f(1,:,1),f(2,:,2),f(2,:,1),f(1,:,2),v,t,dh,dho) ! y: minmin
         case (9);  call app_CC(f(1,1,:),f(2,2,:),f(2,1,:),f(1,2,:),v,t,dh,dho) ! z: minmin
         
         case (2);  call app_CC(f(:,1,s(3)),f(:,2,s(3)-1),f(:,2,s(3)),f(:,1,s(3)-1),v,t,dh,dho) ! x: minmax
         case (6);  call app_CC(f(1,:,s(3)),f(2,:,s(3)-1),f(2,:,s(3)),f(1,:,s(3)-1),v,t,dh,dho) ! y: minmax
         case (10); call app_CC(f(1,s(2),:),f(2,s(2)-1,:),f(2,s(2),:),f(1,s(2)-1,:),v,t,dh,dho) ! z: minmax

         case (3);  call app_CC(f(:,s(2),1),f(:,s(2)-1,2),f(:,s(2)-1,1),f(:,s(2),2),v,t,dh,dho) ! x: maxmin
         case (7);  call app_CC(f(s(1),:,1),f(s(1)-1,:,2),f(s(1)-1,:,1),f(s(1),:,2),v,t,dh,dho) ! y: maxmin
         case (11); call app_CC(f(s(1),1,:),f(s(1)-1,2,:),f(s(1)-1,1,:),f(s(1),2,:),v,t,dh,dho) ! z: maxmin

         case (4);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! x: maxmax
         case (8);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! y: maxmax
         case (12); call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! z: maxmax
         end select

         select case (e) ! edge
         case (1);  call app_CC(f(:,1,1),f(:,2,2),f(:,2,1),f(:,1,2),v,t,dh,dho) ! x: minmin
         case (5);  call app_CC(f(1,:,1),f(2,:,2),f(2,:,1),f(1,:,2),v,t,dh,dho) ! y: minmin
         case (9);  call app_CC(f(1,1,:),f(2,2,:),f(2,1,:),f(1,2,:),v,t,dh,dho) ! z: minmin
         
         case (2);  call app_CC(f(:,1,d),f(:,2,d-1),f(:,2,d),f(:,1,d-1),v,t,dh,dho) ! x: minmax
         case (6);  call app_CC(f(1,:,d),f(2,:,d-1),f(2,:,d),f(1,:,d-1),v,t,dh,dho) ! y: minmax
         case (10); call app_CC(f(1,d,:),f(2,d-1,:),f(2,d,:),f(1,d-1,:),v,t,dh,dho) ! z: minmax

         case (3);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! x: maxmin
         case (7);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! y: maxmin
         case (11); call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! z: maxmin

         case (4);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! x: maxmax
         case (8);  call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! y: maxmax
         case (12); call app_CC(f(:,:,:),f(:,:,:),f(:,:,:),f(:,:,:),v,t,dh,dho) ! z: maxmax
         end select
         
       end subroutine

       subroutine apply_edge_CC(ug,ui,ug1,ug2,bvals)
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: ui,ug1,ug2,bvals
         ug = 4.0_cp*bvals - (ui + ug1 + ug2)
       end subroutine

       subroutine apply_edge_dir(ug,ui,ui_opp,bvals,bctype,dh)
         implicit none
         real(cp),intent(inout),dimension(:) :: ug,ui,ui_opp
         real(cp),dimension(:),intent(in) :: bvals
         real(cp),intent(in) :: dh
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ug = 2.0_cp*bvals - ui ! Dirichlet - interpolated - wall incoincident
         ! *************************** NEUMANN *****************************
         case (2); ug = ui + dh*bvals     ! Implicit Neumann - interpolated - wall incoincident ~O(dh)
         ! *************************** PERIODIC *****************************
         case (3); ug = ui_opp            ! Periodic - interpolated - wall incoincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in apply_edges.f90'
         end select
       end subroutine

       subroutine apply_edge_Node(ug,ub,ug1,ug2,bvals)
         implicit none
         real(cp),dimension(:),intent(inout) :: ug
         real(cp),dimension(:),intent(in) :: ub,ug1,ug2,bvals
         ug = 4.0_cp*bvals - (ub + ug1 + ug2)
       end subroutine

       subroutine apply_edge_Node_dir(ug,ub,ui,ui_opp,bvals,bctype,dh)
         implicit none
         real(cp),intent(inout),dimension(:) :: ug,ub,ui,ui_opp
         real(cp),dimension(:),intent(in) :: bvals
         real(cp),intent(in) :: dh
         integer,intent(in) :: bctype
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1); ub = bvals; ug = 2.0_cp*bvals - ui ! Dirichlet - direct - wall coincident
         ! *************************** NEUMANN *****************************
         case (3); ub = ui; ug = ui                   ! Explicit Neumann - direct - wall coincident ~O(dh)?
         case (4); ug = ui - 2.0_cp*bvals*dh          ! Implicit Neumann - direct - wall coincident ~O(dh^2)
         ! *************************** PERIODIC *****************************
         case (6); ub = unb_opp; ug = ui_opp          ! Periodic - direct - wall coincident ~O(dh)
         case default
         stop 'Error: Bad bctype! Caught in apply_edges.f90'
         end select
       end subroutine

       end module