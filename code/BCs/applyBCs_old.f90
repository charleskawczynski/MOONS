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
       use mesh_mod
       implicit none

       private
       public :: applyAllBCs
       ! public :: applyAllGhostBCs

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       interface applyAllGhostBCs;  module procedure applyAllGhost_SF;   end interface
       interface applyAllGhostBCs;  module procedure applyAllGhost_VF;   end interface


       interface applyAllBCs;       module procedure applyAllBCs_RF;     end interface
       interface applyAllBCs;       module procedure applyAllBCs_VF;     end interface
       interface applyAllBCs;       module procedure applyAllBCs_VF2;    end interface
       interface applyAllBCs;       module procedure applyAllBCs_SF;     end interface
       interface applyAllBCs;       module procedure applyAllBCs_SF2;    end interface
       interface applyBCs;          module procedure applyBCs;           end interface

       contains

       subroutine applyAllBCs_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call applyAllBCs(U%x,m)
         call applyAllBCs(U%y,m)
         call applyAllBCs(U%z,m)
       end subroutine

       subroutine applyAllBCs_VF2(U,m,B)
         implicit none
         type(VF),intent(inout) :: U
         type(VF),intent(in) :: B
         type(mesh),intent(in) :: m
         call applyAllBCs(U%x,m,B%x)
         call applyAllBCs(U%y,m,B%y)
         call applyAllBCs(U%z,m,B%z)
       end subroutine

       subroutine applyAllGhost_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call applyAllGhostBCs(U%x,m)
         call applyAllGhostBCs(U%y,m)
         call applyAllGhostBCs(U%z,m)
       end subroutine

       subroutine applyAllBCs_SF2(U,m,B)
         implicit none
         type(SF),intent(inout) :: U
         type(SF),intent(in) :: B
         type(mesh),intent(in) :: m
         integer :: i
         if (m%s.gt.1) then ! Check for stitching
           do i=1,m%s
             if (.not.m%g(i)%st_face%hmin(2)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),3,2)
             if (.not.m%g(i)%st_face%hmax(2)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),4,2)
             if (.not.m%g(i)%st_face%hmin(1)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),1,1)
             if (.not.m%g(i)%st_face%hmax(1)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),2,1)
             if (.not.m%g(i)%st_face%hmin(3)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),5,3)
             if (.not.m%g(i)%st_face%hmax(3)) call applyBC_Face_dir(U%RF(i)%f,B%RF(i)%b,m%g(i),6,3)
           enddo
         else
           do i=1,m%s
             call applyAllBCs(U%RF(i)%f,B%RF(i)%b,m%g(i))
           enddo
         endif
       end subroutine

       subroutine applyAllBCs_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i
         if (m%s.gt.1) then ! Check for stitching
           do i=1,m%s
             if (.not.m%g(i)%st_face%hmin(2)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),3,2)
             if (.not.m%g(i)%st_face%hmax(2)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),4,2)
             if (.not.m%g(i)%st_face%hmin(1)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),1,1)
             if (.not.m%g(i)%st_face%hmax(1)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),2,1)
             if (.not.m%g(i)%st_face%hmin(3)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),5,3)
             if (.not.m%g(i)%st_face%hmax(3)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),6,3)
           enddo
         else
           do i=1,m%s
             call applyAllBCs(U%RF(i)%f,U%RF(i)%b,m%g(i))
           enddo
         endif
       end subroutine

       subroutine applyAllGhost_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: i
         if (m%s.gt.1) then ! Check for stitching
           do i=1,m%s
             if (CC_along(U,2)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),3,2)
             if (CC_along(U,2)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),4,2)
             if (CC_along(U,1)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),1,1)
             if (CC_along(U,1)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),2,1)
             if (CC_along(U,3)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),5,3)
             if (CC_along(U,3)) call applyBC_Face_dir(U%RF(i)%f,U%RF(i)%b,m%g(i),6,3)
           enddo
         endif
       end subroutine

       subroutine applyAllBCs_RF(u,b,g)
        ! Note that these boundary conditions are applied in a NON- arbitrary
        ! order and changing them WILL change the outcome of the results.
        ! Consider changing BCs to only affect interior data.
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(BCs),intent(in) :: b
         type(grid),intent(in) :: g
         call applyBC_Face_dir(u,b,g,3,2)
         call applyBC_Face_dir(u,b,g,4,2)
         call applyBC_Face_dir(u,b,g,1,1)
         call applyBC_Face_dir(u,b,g,2,1)
         call applyBC_Face_dir(u,b,g,5,3)
         call applyBC_Face_dir(u,b,g,6,3)
       end subroutine

       subroutine applyBC_Face_dir(u,b,g,f,d)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: u
         type(BCs),intent(in) :: b
         type(grid),intent(in) :: g
         integer,intent(in) :: f,d ! face,dir

#ifdef _DEBUG_APPLYBCS_
         if (.not.b%defined) then
          call print_defined(b)
          stop 'Error: BCs not fully defined..'
         endif
#endif
         call applyBCs(u,b%f(f)%bctype,f,b%f(f)%vals,g%c(d)%hn,g%c(d)%hc,b%s(d))
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
           select case (face) ! Looks good...
           case (1); u(1,:,:) = u(3,:,:)   !; u(2,:,:) = u(3,:,:) ! This second part seems to not make sense
           case (3); u(:,1,:) = u(:,3,:)   !; u(:,2,:) = u(:,3,:) ! This second part seems to not make sense
           case (5); u(:,:,1) = u(:,:,3)   !; u(:,:,2) = u(:,:,3) ! This second part seems to not make sense
           case (2); u(s,:,:) = u(s-2,:,:) !; u(s-1,:,:) = u(s-2,:,:) ! This second part seems to not make sense
           case (4); u(:,s,:) = u(:,s-2,:) !; u(:,s-1,:) = u(:,s-2,:) ! This second part seems to not make sense
           case (6); u(:,:,s) = u(:,:,s-2) !; u(:,:,s-1) = u(:,:,s-2) ! This second part seems to not make sense
           end select
         case (11) ! Explicit Neumann - direct - wall coincident ~O(dh)?
           select case (face) ! Trying this...
           case (1); u(1,:,:) = u(2,:,:)   ! For implicit
           case (3); u(:,1,:) = u(:,2,:)   ! For implicit
           case (5); u(:,:,1) = u(:,:,2)   ! For implicit
           case (2); u(s,:,:) = u(s-1,:,:) ! For implicit
           case (4); u(:,s,:) = u(:,s-1,:) ! For implicit
           case (6); u(:,:,s) = u(:,:,s-1) ! For implicit
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
         ! *************************** ANTI-SYMMETRIC *****************************
         case (9) ! Anti-symmetric - direct - wall coincident
           select case (face)
           case (1); u(2,:,:) =   0.0_cp; u(1,:,:) = -u(3,:,:)
           case (3); u(:,2,:) =   0.0_cp; u(:,1,:) = -u(:,3,:)
           case (5); u(:,:,2) =   0.0_cp; u(:,:,1) = -u(:,:,3)
           case (2); u(s-1,:,:) = 0.0_cp; u(s,:,:) = -u(s-2,:,:)
           case (4); u(:,s-1,:) = 0.0_cp; u(:,s,:) = -u(:,s-2,:)
           case (6); u(:,:,s-1) = 0.0_cp; u(:,:,s) = -u(:,:,s-2)
           end select
         case (10) ! Anti-symmetric - direct - wall incoincident
           select case (face)
           case (1); u(1,:,:) = -u(2,:,:)
           case (3); u(:,1,:) = -u(:,2,:)
           case (5); u(:,:,1) = -u(:,:,2)
           case (2); u(s,:,:) = -u(s-1,:,:)
           case (4); u(:,s,:) = -u(:,s-1,:)
           case (6); u(:,:,s) = -u(:,:,s-1)
           end select
         case default
         stop 'Error: Bad bctype! Caught in applyBCs.f90'
         end select
       end subroutine

       end module