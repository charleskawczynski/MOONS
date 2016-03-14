       module E_K_budget_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use E_K_budget_terms_mod
       implicit none

       private
       public :: E_K_Budget

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

       subroutine E_K_Budget(e_budget,U,Unm1,U_CC,B,B0,J,p,m,dt,&
         temp_F1,temp_F2,temp_F_TF,temp_CC1_TF,temp_CC2_TF)
         implicit none
         type(VF),intent(in) :: U,Unm1,U_CC,B,B0,J
         type(SF),intent(in) :: p
         type(VF),intent(inout) :: temp_F1,temp_F2
         type(TF),intent(inout) :: temp_F_TF,temp_CC1_TF,temp_CC2_TF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dt
         real(cp),dimension(5),intent(inout) :: e_budget
         call unsteady(e_budget(1),U,Unm1,dt,m,temp_F1,temp_CC1_TF%x)
         call Convection(e_budget(2),U,U_CC,m,temp_F1,temp_F2,temp_CC1_TF%x,temp_CC1_TF%x%x)
         call Transport(e_budget(3),U,p,m,temp_CC1_TF%x%x,temp_F1,temp_F2,temp_F_TF)
         call add(temp_F1,B,B0)
         call Lorentz(e_budget(4),J,temp_F1,U_CC,m,temp_CC1_TF%x,temp_CC1_TF%y,temp_CC1_TF%z,temp_F_TF%x)
         call Viscous_Dissipation(e_budget(5),U_CC,m,temp_CC1_TF,temp_CC2_TF)
       end subroutine

       end module