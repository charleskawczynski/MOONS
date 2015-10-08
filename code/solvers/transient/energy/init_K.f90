       module init_K_mod
       use simParams_mod
       use mesh_mod
       use ops_embedExtract_mod
       use domain_mod
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

       subroutine initK(k,D,g)
         implicit none
         type(mesh),intent(in) :: g
         type(domain),intent(in) :: D
         type(SF),intent(inout) :: k
         if (preDefined_K.ne.0) then
           call initPredefinedK(k,D,g)
         else
           call initUserK(k,D,g)
         endif
       end subroutine

       subroutine initPredefinedK(k,D,g)
         implicit none
         type(SF),intent(inout) :: k
         type(domain),intent(in) :: D
         type(mesh),intent(in) :: g
         type(SF) :: k_l
         call init_CC(k_l,D%m_in) ! SD%g = Interior mesh
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,D)
         call delete(k_l)
       end subroutine

       subroutine initUserK(k,D,g)
         implicit none
         type(SF),intent(inout) :: k
         type(domain),intent(in) :: D
         type(mesh),intent(in) :: g
         type(SF) :: k_l
         call init_CC(k_l,D%m_in) ! D%g = Interior mesh
         call assign(k_l,1.0_cp)
         call assign(k,kStarWall)
         call embedCC(k,k_l,D)
         call delete(k_l)
       end subroutine

       end module
