       module clean_B_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_discrete_complex_mod
       use applyBCs_mod
       use norms_mod
       use SOR_mod

       implicit none

       private
       public :: FRM_CT
       public :: LRM_CT

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

       subroutine clean_B_SOR(SOR,B_F,phi,m,n,compute_norms,tmp_F,tmp_CC)
         implicit none
         type(SORSolver),intent(inout) :: SOR
         type(VF),intent(inout) :: B_F,tmp_F
         type(SF),intent(inout) :: phi,tmp_CC
         type(grid),intent(in) :: m
         integer,intent(in) :: n
         logical,intent(in) :: compute_norms
         call div(tmp_CC,B_F,m)
         call solve(SOR,phi,tmp_CC,m,n,compute_norms)
         call grad(tmp_F,phi,m)
         call subtract(B_F,tmp_F)
         call applyAllBCs(B_F,m)
       end subroutine

       subroutine clean_B_CG(CG,B,phi,m,n,compute_norms,tmp_F,tmp_CC,tmp_CC_VF)
         implicit none
         type(CGSolver),intent(inout) :: CG
         type(VF),intent(inout) :: B,tmp_F,tmp_CC_VF
         type(SF),intent(inout) :: phi,tmp_CC
         type(grid),intent(in) :: m
         integer,intent(in) :: n
         logical,intent(in) :: compute_norms
         call div(tmp_CC,B,m)
         call solve(CG,phi,tmp_CC,m,n,compute_norms)
         call grad(tmp_F,phi,m)
         call face2CellCenter(tmp_CC_VF,tmp_F,m)
         call subtract(B,tmp_CC_VF)
         call applyAllBCs(B,m)
       end subroutine

       end module