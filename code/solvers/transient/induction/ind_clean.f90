       module ind_clean_mod
       use grid_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use applyBCs_mod

       implicit none

       private
       public :: ind_clean,init,delete,solve
       ! public :: print,export

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       type ind_clean
         type(SF) :: phi,temp
         integer :: N_iter
       end type

       interface init;    module procedure init_ind_clean;    end interface
       interface delete;  module procedure delete_ind_clean;  end interface
       interface clean;   module procedure apply_cleaning;    end interface

       contains

       ! ******************* INIT/DELETE ***********************

       subroutine init_ind_clean(c,g)
         implicit none
         type(ind_clean),intent(inout) :: c
         type(grid),intent(in) :: g
         call init_CC(c%phi,g)
       end subroutine

       subroutine deleteind_clean(c)
         implicit none
         type(ind_clean),intent(inout) :: c
         call delete(c%phi)
       end subroutine

       subroutine apply_cleaning(c,B,g)
         ! Applies cleaning prcedure to magnetic field B
         implicit none
         type(ind_clean),intent(inout) :: c
         type(VF),intent(in) :: B
         call 
         call applyAllBCs(B,g)
       end subroutine

       subroutine apply_cleaning(c,B,temp_CC,temp_F,g,ss_MHD)
         implicit none
         type(ind_clean),intent(inout) :: c
         type(VF),intent(inout) :: B
         type(grid),intent(in) :: g
         type(solverSettings),intent(in) :: ss_MHD
         call div(c%temp,c%B,g)

         call poisson(c%SOR,c%phi,c%temp,g,c%ss_cleanB,c%err_cleanB,.false.)

         call grad(temp_F,c%phi,g)
         call subtract(B,ind%temp_F)

         call face2CellCenter(ind%B,ind%B_face,g)

         call applyAllBCs(ind%B,g)
       end subroutine


       end module