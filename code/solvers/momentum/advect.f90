       module advect_mod
       use mesh_mod
       use VF_mod
       use ops_discrete_mod
       use ops_physics_mod
       
       implicit none
       private
       public :: advect

#ifdef _ADVECT_CD2_AB2_
       interface advect;      module procedure advect_CD2_AB2;          end interface
#endif
#ifdef _ADVECT_CD2_EXPLICIT_
       interface advect;      module procedure advect_CD2_explicit;     end interface
#endif

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

       subroutine advect_CD2_AB2(adv,U,Unm1,m,n,temp_E1,temp_E2,temp_CC,temp_F)
         ! Computes
         !      -0.5(3∇•(uu)ⁿ - ∇•(uu)ⁿ⁻¹)
         implicit none
         type(VF),intent(inout) :: adv
         type(VF),intent(in) :: U,Unm1
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_E1,temp_E2,temp_CC,temp_F
         integer,intent(in) :: n
         if (n.gt.1) then
           call faceAdvectDonor(adv,Unm1,Unm1,temp_E1,temp_E2,temp_CC,m)
           call multiply(adv,0.5_cp)
           call faceAdvectDonor(temp_F,U,U,temp_E1,temp_E2,temp_CC,m)
           call multiply(temp_F,-3.0_cp/2.0_cp)
           call add(adv,temp_F)
         else
           call faceAdvectDonor(adv,U,U,temp_E1,temp_E2,temp_CC,m)
           call multiply(adv,-1.0_cp)
         endif
       end subroutine

       subroutine advect_CD2_explicit(adv,U,m,temp_CC,temp_E1,temp_E2)
         ! Computes
         !      -∇•(uu)
         implicit none
         type(VF),intent(inout) :: U,temp_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: temp_E1,temp_E2
         call faceAdvectDonor(adv,U,U,temp_E1,temp_E2,temp_CC,m)
         call multiply(adv,-1.0_cp)
       end subroutine

       end module