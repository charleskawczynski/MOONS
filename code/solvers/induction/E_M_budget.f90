       module E_M_budget_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use E_M_budget_terms_mod
       implicit none

       private
       public :: E_M_Budget

       contains

       subroutine E_M_Budget(e_budget,B,Bnm1,B0,B0nm1,J,sigmaInv_F,sigmaInv_CC,U,U_CC,&
         m,dt,temp_CC_TF,temp_CC_VF,temp_F1,temp_F2,temp_F1_TF,temp_F2_TF,temp_F3_TF)
         implicit none
         type(VF),intent(in) :: B,Bnm1,B0,B0nm1,J,sigmaInv_F,U,U_CC
         type(SF),intent(in) :: sigmaInv_CC
         type(VF),intent(inout) :: temp_F1,temp_F2,temp_CC_VF
         type(TF),intent(inout) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         real(cp),dimension(5),intent(inout) :: e_budget
         logical :: debug
         debug = .false.
         
         if (debug) write(*,*) 'got here 1'
         call add(temp_F2,B,B0)
         if (debug) write(*,*) 'got here 2'
         call add(temp_F1_TF%y,Bnm1,B0nm1)
         if (debug) write(*,*) 'got here 3'
         call unsteady(e_budget(1),temp_F2,temp_F1_TF%y,dt,m,temp_F1_TF%z,temp_CC_TF%x)
         if (debug) write(*,*) 'got here 4'
         call Poynting(e_budget(2),temp_F2,J,U,sigmaInv_F,m,temp_CC_TF%x%x,temp_F1,temp_F1_TF,temp_F2_TF,temp_F3_TF)
         if (debug) write(*,*) 'got here 5'
         call Joule_Dissipation(e_budget(3),J,sigmaInv_CC,m,temp_CC_TF%x,temp_F1_TF%y)

         ! call Lorentz(e_budget(4),J,temp_F2,U_CC,m,temp_CC_TF%x,temp_CC_TF%y,temp_CC_TF%z,temp_F1_TF%y)
         if (debug) write(*,*) 'got here 6'
         call maxwell_stress(e_budget(4),temp_F2,U_CC,m,temp_CC_VF,temp_CC_TF)
         if (debug) write(*,*) 'got here 7'
         call E_M_Convection(e_budget(5),temp_F2,U_CC,m,temp_CC_TF%x,temp_CC_TF%y%y)
         if (debug) write(*,*) 'got here 8'
       end subroutine

       end module