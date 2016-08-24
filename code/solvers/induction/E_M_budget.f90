       module E_M_budget_mod
       use current_precision_mod
       use mesh_mod
       use string_mod
       use path_mod
       use dir_tree_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use E_M_budget_terms_mod
       implicit none

       private
       public :: E_M_Budget

       contains

       subroutine E_M_Budget(DT,e_integral,B,Bnm1,B0,B0nm1,J,sigmaInv_F,sigmaInv_CC,U,&
         m,dTime,TF_CC,VF_CC,VF_F1,VF_F2,TF_F1,TF_F2,TF_F3)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: B,Bnm1,B0,B0nm1,J,sigmaInv_F,U
         type(SF),intent(in) :: sigmaInv_CC
         type(VF),intent(inout) :: VF_F1,VF_F2,VF_CC
         type(TF),intent(inout) :: TF_CC,TF_F1,TF_F2,TF_F3
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime
         real(cp),dimension(3),intent(inout) :: e_integral
         integer :: i
         type(SF) :: e
         call init(e,sigmaInv_CC)
         i = 1
         
         call add(VF_F2,B,B0)
         call add(TF_F1%y,Bnm1,B0nm1)
         call export_Unsteady(e_integral(i),e,VF_F2,TF_F1%y,dTime,m,TF_CC%x,TF_CC%y,DT); i=i+1
         call export_Joule_Dissipation(e_integral(i),e,J,sigmaInv_CC,m,TF_CC%x,VF_F1,DT); i=i+1
         call export_Poynting(e_integral(i),e,VF_F2,J,U,sigmaInv_F,m,VF_CC%x,VF_F1,TF_F1,TF_F2,TF_F3,DT); i=i+1
         call delete(e)
       end subroutine

       end module