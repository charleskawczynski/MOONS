       module init_K_mod
       use current_precision_mod
       use mesh_mod
       use ops_embedExtract_mod
       use domain_mod
       use SF_mod
       implicit none

       private
       public :: initK

       integer :: preDefined_K = 1 ! k* = k_wall/k_l
       !                                  0 : User-defined case (no override)
       !                                  1 : k* = kStar
       real(cp) :: kStarWall = 1.0_cp ! k* = k_wall/k_l

       contains

       subroutine initK(k,m,D)
         implicit none
         type(domain),intent(in) :: D
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: k
         if (preDefined_K.ne.0) then
           call initPredefinedK(k,m,D)
         else
           call initUserK(k,m,D)
         endif
       end subroutine

       subroutine initPredefinedK(k,m,D)
         implicit none
         type(SF),intent(inout) :: k
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         type(SF) :: k_l
         call init_CC(k_l,m,D)
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,D)
         call delete(k_l)
       end subroutine

       subroutine initUserK(k,m,D)
         implicit none
         type(SF),intent(inout) :: k
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D
         type(SF) :: k_l
         call init_CC(k_l,m,D)
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,D)
         call delete(k_l)
       end subroutine

       end module
