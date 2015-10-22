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
       use apply_faces_mod
       use apply_edges_mod
       use apply_corners_mod
       use grid_mod
       use mesh_mod
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


       interface applyAllBCs;       module procedure applyAllBCs_VF;     end interface
       interface applyAllBCs;       module procedure applyAllBCs_SF;     end interface

       contains

       subroutine applyAllBCs_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call applyAllBCs(U%x,m)
         call applyAllBCs(U%y,m)
         call applyAllBCs(U%z,m)
       end subroutine

       subroutine applyAllBCs_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
         integer :: k
         do k=1,3; call apply_faces(U,m,k); enddo
         do k=1,3; call apply_edges(U,m,k); enddo
         ! do k=1,3; call apply_corner(U,m,k); enddo
       end subroutine

       end module