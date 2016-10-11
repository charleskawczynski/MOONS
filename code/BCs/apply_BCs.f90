       module apply_BCs_mod
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
       ! Pre-processor directives: (_DEBUG_apply_BCs_)
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

       use current_precision_mod
       use SF_mod
       use VF_mod
       use check_BCs_mod
       use apply_BCs_faces_mod
       use mesh_mod
       implicit none

       private
       public :: apply_BCs

       interface apply_BCs;    module procedure apply_BCs_VF;                 end interface
       interface apply_BCs;    module procedure apply_BCs_SF;                 end interface

       contains

       subroutine apply_BCs_VF(U,m)
         implicit none
         type(VF),intent(inout) :: U
         type(mesh),intent(in) :: m
         call apply_BCs(U%x,m)
         call apply_BCs(U%y,m)
         call apply_BCs(U%z,m)
       end subroutine

       subroutine apply_BCs_SF(U,m)
         implicit none
         type(SF),intent(inout) :: U
         type(mesh),intent(in) :: m
#ifdef _DEBUG_APPLY_BCS_
       call check_defined(U)
#endif
         ! if (m%s.gt.1) call apply_stitches_faces(U,m)
         ! if (m%s.gt.1) call apply_stitches_edges(U,m)
         ! if (m%s.gt.1) call apply_stitches_corners(U,m)
         call apply_BCs_faces(U,m)
         ! if (m%s.gt.1) call apply_BCs_edges(U,m)
         ! call apply_BCs_coners(U,m)
       end subroutine

       end module