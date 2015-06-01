       module initializeK_mod
       use simParams_mod
       use grid_mod
       use ops_embedExtract_mod
       implicit none

       private
       public :: initK

       ! This gets overridden by benchmarkCase
       integer,parameter :: preDefined_K = 0
       !                                   0 : User-defined case (no override)
       !                                   1 : k = 1 (uniform)

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

       subroutine initK(k,g)
         implicit none
         type(grid),intent(in) :: g
         real(cp),dimension(:,:,:),intent(inout) :: k
         if (benchmarkCase.ne.0) then
           call initBenchmarkK(k)
         elseif (preDefined_K.ne.0) then
           call initPredefinedK(k)
         else
           call initUserK(k)
         endif
       end subroutine

       subroutine initBenchmarkK(k)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: k
         k = real(1.0,cp)
       end subroutine

       subroutine initPredefinedK(k)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: k
         k = real(1.0,cp)
       end subroutine

       subroutine initUserK(k)
         implicit none
         real(cp),dimension(:,:,:),intent(inout) :: k
         k = real(1.0,cp)
       end subroutine

       end module
