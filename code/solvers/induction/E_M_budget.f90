       module E_M_budget_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use E_M_budget_terms_mod
       implicit none

       private
       public :: E_M_Budget

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

       subroutine E_M_Budget(e_budget,B,Bnm1,B0,B0nm1,J,sigmaInv_F,sigmaInv_CC,U,U_CC,&
         m,dt,temp_CC_TF,temp_F1,temp_F2,temp_F1_TF,temp_F2_TF,temp_F3_TF)
         implicit none
         type(VF),intent(in) :: B,Bnm1,B0,B0nm1,J,sigmaInv_F,U,U_CC
         type(SF),intent(in) :: sigmaInv_CC
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(TF),intent(inout) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         real(cp),dimension(4),intent(inout) :: e_budget
         write(*,*) 'got here 1'
         call add(temp_F2,B,B0)
         write(*,*) 'got here 2'
         call add(temp_F1_TF%y,Bnm1,B0nm1)
         write(*,*) 'got here 3'
         call unsteady(e_budget(1),temp_F2,temp_F1_TF%y,dt,m,temp_F1_TF%z,temp_CC_TF%x)
         write(*,*) 'got here 4'
         call Lorentz(e_budget(2),J,temp_F2,U_CC,m,temp_CC_TF%x,temp_CC_TF%y,temp_CC_TF%z,temp_F1_TF%y)
         write(*,*) 'got here 5'
         call Joule_Dissipation(e_budget(3),J,sigmaInv_CC,m,temp_CC_TF%x,temp_F1_TF%y)
         write(*,*) 'got here 6'
         call Poynting(e_budget(4),temp_F2,J,U,sigmaInv_F,m,temp_CC_TF%x%x,temp_F1,temp_F1_TF,temp_F2_TF,temp_F3_TF)
         write(*,*) 'got here 7'
       end subroutine

       end module