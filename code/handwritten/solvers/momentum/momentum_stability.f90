       module momentum_stability_mod
       use current_precision_mod
       use mesh_extend_mod
       use SF_extend_mod

       implicit none
       private

       public :: momentum_stability
       public :: init,delete

       type momentum_stability
         type(SF) :: Fo_grid
         type(SF) :: Co_grid
         type(SF) :: Re_grid
         type(SF) :: KE_adv
         type(SF) :: KE_diff
         type(SF) :: KE_pres
         type(SF) :: KE_transient
         type(SF) :: KE_jCrossB
       end type

       interface init;       module procedure init_MS;        end interface
       interface delete;     module procedure delete_MS;      end interface

       contains

       ! **********************************************************
       ! ********************* ESSENTIALS *************************
       ! **********************************************************

       subroutine init_MS(MS,m)
         implicit none
         type(momentum_stability),intent(inout) :: MS
         type(mesh),intent(in) :: m
         call init_CC(MS%Fo_grid     ,m,0.0_cp)
         call init_CC(MS%Co_grid     ,m,0.0_cp)
         call init_CC(MS%Re_grid     ,m,0.0_cp)
         call init_CC(MS%KE_adv      ,m,0.0_cp)
         call init_CC(MS%KE_diff     ,m,0.0_cp)
         call init_CC(MS%KE_pres     ,m,0.0_cp)
         call init_CC(MS%KE_transient,m,0.0_cp)
         call init_CC(MS%KE_jCrossB  ,m,0.0_cp)
       end subroutine

       subroutine delete_MS(MS)
         implicit none
         type(momentum_stability),intent(inout) :: MS
         call delete(MS%Fo_grid)
         call delete(MS%Co_grid)
         call delete(MS%Re_grid)
         call delete(MS%KE_adv)
         call delete(MS%KE_diff)
         call delete(MS%KE_pres)
         call delete(MS%KE_transient)
         call delete(MS%KE_jCrossB)
       end subroutine

       end module