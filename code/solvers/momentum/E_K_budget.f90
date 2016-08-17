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

       subroutine E_K_Budget(DT,e_integral,U,Unm1,U_CC,B,B0,J,p,m,dTime,&
         VF_F1,VF_F2,TF_CC1,TF_CC2)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: U,Unm1,U_CC,B,B0,J
         type(SF),intent(in) :: p
         type(VF),intent(inout) :: VF_F1,VF_F2
         type(TF),intent(inout) :: TF_CC1,TF_CC2
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime
         real(cp),dimension(8),intent(inout) :: e_integral
         type(SF) :: e
         integer :: i
         call init(e,TF_CC1%x%x)
         i = 1
         call Unsteady_export(e_integral(i),e,U,Unm1,dTime,m,TF_CC1%x,TF_CC2%x,DT); i=i+1
         call E_K_Convection_export(e_integral(i),e,U,U_CC,m,VF_F1,VF_F2,TF_CC1%x,TF_CC2%x%x,DT); i=i+1
         call E_K_Diffusion_export(e_integral(i),e,U_CC,m,TF_CC1%x,DT); i=i+1
         call E_K_Pressure_export(e_integral(i),e,U,p,m,VF_F1,DT); i=i+1
         call Viscous_Dissipation_export(e_integral(i),e,U_CC,m,TF_CC1,TF_CC2,DT); i=i+1
         call add(VF_F2,B,B0)
         call E_M_Convection_export(e_integral(i),e,VF_F2,U,m,TF_CC1%x,TF_CC2%x,VF_F1,DT); i=i+1
         call E_M_Tension_export(e_integral(i),e,VF_F2,U_CC,m,TF_CC1%x,TF_CC1%y,TF_CC1%z,TF_CC2,DT); i=i+1
         call Lorentz_export(e_integral(i),e,J,VF_F2,U_CC,m,TF_CC1%x,TF_CC1%y,TF_CC1%z,VF_F1,DT); i=i+1
         call delete(e)
       end subroutine

       end module