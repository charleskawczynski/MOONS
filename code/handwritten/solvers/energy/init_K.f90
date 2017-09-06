       module init_K_mod
       use current_precision_mod
       use mesh_extend_mod
       use ops_embedExtract_mod
       use mesh_domain_mod
       use SF_extend_mod
       implicit none

       private
       public :: initK

       integer :: preDefined_K = 1 ! k* = k_wall/k_l
       !                                  0 : User-defined case (no override)
       !                                  1 : k* = kStar
       real(cp) :: kStarWall = 1.0_cp ! k* = k_wall/k_l

       contains

       subroutine initK(k,m,MD)
         implicit none
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: k
         type(mesh_domain),intent(in) :: MD
         if (preDefined_K.ne.0) then
           call initPredefinedK(k,m,MD)
         else
           call initUserK(k,m,MD)
         endif
       end subroutine

       subroutine initPredefinedK(k,m,MD)
         implicit none
         type(SF),intent(inout) :: k
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(SF) :: k_l
         call init_CC(k_l,m,MD)
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,MD)
         call delete(k_l)
       end subroutine

       subroutine initUserK(k,m,MD)
         implicit none
         type(SF),intent(inout) :: k
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         type(SF) :: k_l
         call init_CC(k_l,m,MD)
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,MD)
         call delete(k_l)
       end subroutine

       end module
