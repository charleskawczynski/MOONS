       module initializeK_mod
       use simParams_mod
       use grid_mod
       use ops_embedExtract_mod
       use SF_mod
       implicit none

       private
       public :: initK


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif


       ! This gets overridden by benchmarkCase
       integer,parameter :: preDefined_K = 1 ! k* = k_wall/k_l
       !                                       0 : User-defined case (no override)
       !                                       1 : k* = kStar
       real(cp) :: kStarWall = real(1000.0,cp) ! k* = k_wall/k_l


       contains

       subroutine initK(k,SD,g)
         implicit none
         type(grid),intent(in) :: g
         type(subdomain),intent(in) :: SD
         type(SF),intent(inout) :: k
         if (preDefined_K.ne.0) then
           call initPredefinedK(k,SD,g)
         else
           call initUserK(k,SD,g)
         endif
       end subroutine

       subroutine initPredefinedK(k,SD,g)
         implicit none
         type(SF),intent(inout) :: k
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(SF) :: k_l
         call init(k_l,SD%s)
         call assign(k_l,real(1.0,cp))
         call assign(k,kStarWall)
         call embedCC(k,k_l,SD,g)
         call delete(k_l)
       end subroutine

       subroutine initUserK(k,SD,g)
         implicit none
         type(SF),intent(inout) :: k
         type(subdomain),intent(in) :: SD
         type(grid),intent(in) :: g
         type(SF) :: k_l
         call init(k_l,SD%s)
         call assign(k_l,real(1.0,cp))
         call assign(k,kStarWall)
         call embedCC(k,k_l,SD,g)
         call delete(k_l)
       end subroutine


       end module
