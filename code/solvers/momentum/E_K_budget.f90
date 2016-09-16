       module E_K_budget_mod
       use current_precision_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use E_K_budget_terms_mod
       implicit none

       private
       public :: E_K_Budget

       contains

       subroutine E_K_Budget(DT,e_int,U,Unm1,U_CC,B,B0,J,p,m,dTime,Re,Ha,Rem,&
         VF_F1,VF_F2,TF_CC1,TF_CC2)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: U,Unm1,U_CC,B,B0,J
         type(SF),intent(in) :: p
         type(VF),intent(inout) :: VF_F1,VF_F2
         type(TF),intent(inout) :: TF_CC1,TF_CC2
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime,Re,Ha,Rem
         real(cp),dimension(8),intent(inout) :: e_int
         type(SF) :: e
         real(cp) :: scale,Al,Re_inv
         integer :: i
         call init(e,TF_CC1%x%x)
         i = 1
         Al = Ha**2.0_cp/Re/Rem
         Re_inv = 1.0_cp/Re
         scale=1.0_cp;call export_Unsteady(e_int(i),e,U,Unm1,dTime,m,scale,TF_CC1%x,TF_CC2%x,DT); i=i+1
         scale=1.0_cp;call export_E_K_Convection(e_int(i),e,U,U_CC,m,scale,VF_F1,VF_F2,TF_CC1%x,TF_CC2%x%x,DT); i=i+1
         scale=Re_inv;call export_E_K_Diffusion(e_int(i),e,U_CC,m,scale,TF_CC1%x,DT); i=i+1
         scale=1.0_cp;call export_E_K_Pressure(e_int(i),e,U,p,m,scale,VF_F1,DT); i=i+1
         scale=Re_inv;call export_Viscous_Dissipation(e_int(i),e,U_CC,m,scale,TF_CC1,TF_CC2,DT); i=i+1

         call add(VF_F2,B,B0)
         scale=Al;call export_E_M_Convection(e_int(i),e,VF_F2,U,m,scale,TF_CC1%x,TF_CC2%x,VF_F1,DT); i=i+1
         scale=Al;call export_E_M_Tension(e_int(i),e,VF_F2,U_CC,m,scale,TF_CC1%x,TF_CC1%y,TF_CC1%z,TF_CC2,DT); i=i+1
         scale=Al;call export_Lorentz(e_int(i),e,J,VF_F2,U_CC,m,scale,TF_CC1%x,TF_CC1%y,TF_CC1%z,VF_F1,DT); i=i+1
         call delete(e)
       end subroutine

       end module