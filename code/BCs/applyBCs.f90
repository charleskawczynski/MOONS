       module applyBCs_mod
       ! Pre-processor directives: (_DEBUG_APPLYBCS_)
       ! 
       ! Making BCs is a 3 step process:
       ! 
       !       1) Set grid / shape
       !             call init(BCs,g,s)
       !       2) Set type (can use grid information)
       !             call init_Dirichlet(BCs); call init_Dirichlet(BCs,face)
       !             call init_Neumann(BCs);   call init_Neumann(BCs,face)
       !             call init_periodic(BCs);  call init_periodic(BCs,face)
       !       3) Set values
       !             call init(BCs,0.0)       (default)
       !             call init(BCs,0.0,face)
       !             call init(BCs,vals,face)
       ! 
       ! IMPORTANT NOTES:
       ! 
       ! There are two types of Neumann BCs.
       ! 
       ! 1) Explicit Neuamann
       !       - Uses one sided difference stencil to compute 
       !         boundary value, then extrapolates to ghost
       ! 
       ! 2) Implicit Neuamann
       !       - Only computes ghost values
       ! 
       ! Which one to use?
       !     - Use the Explicit Neumann when both
       !              - Data is wall coincident
       !              - Matrix inversion is not used
       !     - Use Implicit Neumann when
       !              - Data is wall incoincident
       ! 

       use SF_mod
       use VF_mod
       use BCs_mod
       use grid_mod
       implicit none

       private
       public :: applyAllBCs

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface applyAllBCs;    module procedure applyAllBCs_RF;     end interface
       interface applyAllBCs;    module procedure applyAllBCs_VF;     end interface
       interface applyAllBCs;    module procedure applyAllBCs_SF;     end interface
       interface applyBCs;       module procedure applyBCs;           end interface

       contains

       subroutine applyAllBCs_VF(U,g)
         implicit none
         type(VF),intent(inout) :: U
         type(grid),intent(in) :: g
         call applyAllBCs(U%x,g)
         call applyAllBCs(U%y,g)
         call applyAllBCs(U%z,g)
       end subroutine

       subroutine applyAllBCs_SF(U,g)
         implicit none
         type(SF),intent(inout) :: U
         type(grid),intent(in) :: g
         integer :: i
         do i=1,U%s
           call applyAllBCs(U%RF(i)%f,U%RF(i)%b,g)
           call applyAllBCs(U%RF(i)%f,U%RF(i)%b,g)
           call applyAllBCs(U%RF(i)%f,U%RF(i)%b,g)
         enddo
       end subroutine

       subroutine applyAllBCs_RF(u,b,g)
        ! Note that these boundary conditions are applied in a NON- arbitrary
        ! order and changing them WILL change the outcome of the results.
        ! Consider changing BCs to only affect interior data.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(BCs),intent(in) :: b
         type(grid),intent(in) :: g
#ifdef _DEBUG_APPLYBCS_
         if (.not.b%defined) then
          call print_defined(b)
          stop 'Error: BCs not fully defined..'
        endif
#endif
         call applyBCs(u,b%face(3)%bctype,3,b%face(3)%vals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         call applyBCs(u,b%face(4)%bctype,4,b%face(4)%vals,g%c(2)%hn,g%c(2)%hc,b%s(2))
         call applyBCs(u,b%face(1)%bctype,1,b%face(1)%vals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyBCs(u,b%face(2)%bctype,2,b%face(2)%vals,g%c(1)%hn,g%c(1)%hc,b%s(1))
         call applyBCs(u,b%face(5)%bctype,5,b%face(5)%vals,g%c(3)%hn,g%c(3)%hc,b%s(3))
         call applyBCs(u,b%face(6)%bctype,6,b%face(6)%vals,g%c(3)%hn,g%c(3)%hc,b%s(3))
       end subroutine

       subroutine applyBCs(u,bctype,face,bvals,hn,hc,s)
         implicit none
         real(cp),intent(inout),dimension(:,:,:) :: u
         real(cp),intent(in),dimension(:) :: hn,hc
         real(cp),dimension(:,:),intent(in) :: bvals
         integer,intent(in) :: bctype,face
         integer,intent(in) :: s
         ! For readability, the faces are traversed in the order:
         !       1 (x_min)
         !       3 (y_min)
         !       5 (z_min)
         !       2 (x_max)
         !       4 (y_max)
         !       6 (z_max)
         select case (bctype)
         ! *************************** DIRICHLET *****************************
         case (1) ! Dirichlet - direct - wall coincident
           ! Assign boundary first, then linearly interpolate to ghost node
           select case (face)
           case (1); u(2,:,:) = bvals;   u(1,:,:) = 2.0_cp*bvals - u(3,:,:)
           case (3); u(:,2,:) = bvals;   u(:,1,:) = 2.0_cp*bvals - u(:,3,:)
           case (5); u(:,:,2) = bvals;   u(:,:,1) = 2.0_cp*bvals - u(:,:,3)
           case (2); u(s-1,:,:) = bvals; u(s,:,:) = 2.0_cp*bvals - u(s-2,:,:)
           case (4); u(:,s-1,:) = bvals; u(:,s,:) = 2.0_cp*bvals - u(:,s-2,:)
           case (6); u(:,:,s-1) = bvals; u(:,:,s) = 2.0_cp*bvals - u(:,:,s-2)
           end select
         case (2) ! Dirichlet - interpolated - wall incoincident
           select case (face)
           case (1); u(1,:,:) = 2.0_cp*bvals - u(2,:,:)
           case (3); u(:,1,:) = 2.0_cp*bvals - u(:,2,:)
           case (5); u(:,:,1) = 2.0_cp*bvals - u(:,:,2)
           case (2); u(s,:,:) = 2.0_cp*bvals - u(s-1,:,:)
           case (4); u(:,s,:) = 2.0_cp*bvals - u(:,s-1,:)
           case (6); u(:,:,s) = 2.0_cp*bvals - u(:,:,s-1)
           end select
         ! *************************** NEUMANN *****************************
         ! The explicit Neumann BCs need to be double checked, I believe the
         ! formulas are correct for only du/dn = 0.
         case (3) ! Explicit Neumann - direct - wall coincident ~O(dh)?
           select case (face)
           case (1); u(2,:,:) = u(3,:,:);     u(1,:,:) = u(3,:,:)
           case (3); u(:,2,:) = u(:,3,:);     u(:,1,:) = u(:,3,:)
           case (5); u(:,:,2) = u(:,:,3);     u(:,:,1) = u(:,:,3)
           case (2); u(s-1,:,:) = u(s-2,:,:); u(s,:,:) = u(s-2,:,:)
           case (4); u(:,s-1,:) = u(:,s-2,:); u(:,s,:) = u(:,s-2,:)
           case (6); u(:,:,s-1) = u(:,:,s-2); u(:,:,s) = u(:,:,s-2)
           end select
         case (4) ! Implicit Neumann - direct - wall coincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = u(3,:,:) - 2.0_cp*bvals*(hn(1)-hn(2))
           case (3); u(:,1,:) = u(:,3,:) - 2.0_cp*bvals*(hn(1)-hn(2))
           case (5); u(:,:,1) = u(:,:,3) - 2.0_cp*bvals*(hn(1)-hn(2))
           case (2); u(s,:,:) = u(s-2,:,:) - 2.0_cp*bvals*(hn(s)-hn(s-1))
           case (4); u(:,s,:) = u(:,s-2,:) - 2.0_cp*bvals*(hn(s)-hn(s-1))
           case (6); u(:,:,s) = u(:,:,s-2) - 2.0_cp*bvals*(hn(s)-hn(s-1))
           end select
         case (5) ! Implicit Neumann - interpolated - wall incoincident ~O(dh)
           select case (face)
           case (1); u(1,:,:) = u(2,:,:) + (hc(1)-hc(2))*bvals
           case (3); u(:,1,:) = u(:,2,:) + (hc(1)-hc(2))*bvals
           case (5); u(:,:,1) = u(:,:,2) + (hc(1)-hc(2))*bvals
           case (2); u(s,:,:) = u(s-1,:,:) + (hc(s)-hc(s-1))*bvals
           case (4); u(:,s,:) = u(:,s-1,:) + (hc(s)-hc(s-1))*bvals
           case (6); u(:,:,s) = u(:,:,s-1) + (hc(s)-hc(s-1))*bvals
           end select
         ! *************************** PERIODIC *****************************
         ! These have not yet been prepared for non-uniform grids:
         case (6) ! Periodic - direct - wall coincident ~O(dh)
           select case (face) ! Wall node
           case (1); u(2,:,:) = u(s-1,:,:)
           case (3); u(:,2,:) = u(:,s-1,:)
           case (5); u(:,:,2) = u(:,:,s-1)
           case (2); u(s-1,:,:) = u(2,:,:)
           case (4); u(:,s-1,:) = u(:,2,:)
           case (6); u(:,:,s-1) = u(:,:,2)
           end select
           select case (face) ! Ghost node
           case (1); u(1,:,:) = u(s-2,:,:)
           case (3); u(:,1,:) = u(:,s-2,:)
           case (5); u(:,:,1) = u(:,:,s-2)
           case (2); u(s,:,:) = u(3,:,:)
           case (4); u(:,s,:) = u(:,3,:)
           case (6); u(:,:,s) = u(:,:,3)
           end select
         case (7) ! Periodic - interpolated - wall incoincident ~O(dh)
           select case (face) ! Ghost cell
           case (1); u(1,:,:) = u(s-1,:,:)
           case (3); u(:,1,:) = u(:,s-1,:)
           case (5); u(:,:,1) = u(:,:,s-1)
           case (2); u(s,:,:) = u(2,:,:)
           case (4); u(:,s,:) = u(:,2,:)
           case (6); u(:,:,s) = u(:,:,2)
           end select
         case (8) ! Periodic - interpolated - wall incoincident ~O(dh^2)
           select case (face)
           case (1); u(1,:,:) = 1.0_cp/3.0_cp*(3.0_cp*u(2,:,:) + u(s-1,:,:) - u(3,:,:))
           case (3); u(:,1,:) = 1.0_cp/3.0_cp*(3.0_cp*u(:,2,:) + u(:,s-1,:) - u(:,3,:))
           case (5); u(:,:,1) = 1.0_cp/3.0_cp*(3.0_cp*u(:,:,2) + u(:,:,s-1) - u(:,:,3))
           case (2); u(s,:,:) = -1.0_cp/3.0_cp*(u(s-2,:,:) - 3.0_cp*u(s-1,:,:) - u(2,:,:))
           case (4); u(:,s,:) = -1.0_cp/3.0_cp*(u(:,s-2,:) - 3.0_cp*u(:,s-1,:) - u(:,2,:))
           case (6); u(:,:,s) = -1.0_cp/3.0_cp*(u(:,:,s-2) - 3.0_cp*u(:,:,s-1) - u(:,:,2))
           end select
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       end module