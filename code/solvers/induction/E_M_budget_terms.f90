       module E_M_budget_terms_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use dir_tree_mod
       use path_mod
       use string_mod
       use export_raw_processed_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use ops_norms_mod
       use norms_mod

       implicit none

       private
       public :: unsteady,         unsteady_export
       public :: Joule_Dissipation,Joule_Dissipation_export
       public :: Poynting,         Poynting_export

       contains

       subroutine Unsteady(e,B,Bnm1,dTime,m,VF_CC1,VF_CC2)
         ! Computes: e = 0.5 ∂_t B•B = 0.5 (B_{n+1}^2 - B_{n}^2)/dTime
         implicit none
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: B,Bnm1
         real(cp),intent(in) :: dTime
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: VF_CC1,VF_CC2
         call face2CellCenter(VF_CC1,B   ,m); call square(VF_CC1)
         call face2CellCenter(VF_CC2,Bnm1,m); call square(VF_CC2)
         call subtract(VF_CC1,VF_CC2)
         call add(e,VF_CC1)
         call multiply(e,0.5_cp/dTime)
       end subroutine
       subroutine Unsteady_export(e_integral,e,B,Bnm1,dTime,m,VF_CC1,VF_CC2,DT)
         implicit none
         real(cp),intent(inout) :: e_integral
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: B,Bnm1
         real(cp),intent(in) :: dTime
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: VF_CC1,VF_CC2
         type(dir_tree),intent(in) :: DT
         call Unsteady(e,B,Bnm1,dTime,m,VF_CC1,VF_CC2)
         call export_raw      (m,e,str(DT%e_budget_C),'B_Unsteady',0)
         call export_processed(m,e,str(DT%e_budget_N),'B_Unsteady',1)
         call Ln(e_integral,e,1.0_cp,m)
       end subroutine

       subroutine Joule_Dissipation(e,J,sigmaInv_CC,m,VF_CC,VF_F)
         ! Computes: j•j/σ
         implicit none
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: J
         type(SF),intent(in) :: sigmaInv_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: VF_CC,VF_F
         call edge2CellCenter(VF_CC,J,m,VF_F)
         call square(VF_CC)
         call multiply(VF_CC,sigmaInv_CC)
         call add(e,VF_CC)
       end subroutine
       subroutine Joule_Dissipation_export(e_integral,e,J,sigmaInv_CC,m,VF_CC,VF_F,DT)
         implicit none
         real(cp),intent(inout) :: e_integral
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: J
         type(SF),intent(in) :: sigmaInv_CC
         type(mesh),intent(in) :: m
         type(VF),intent(inout) :: VF_CC,VF_F
         type(dir_tree),intent(in) :: DT
         call Joule_Dissipation(e,J,sigmaInv_CC,m,VF_CC,VF_F)
         call export_raw      (m,e,str(DT%e_budget_C),'Joule_Dissipation',0)
         call export_processed(m,e,str(DT%e_budget_N),'Joule_Dissipation',1)
         call Ln(e_integral,e,1.0_cp,m)
       end subroutine

       subroutine Poynting(e,B,J,U,sigmaInv_F,m,SF_CC,VF_F,TF_F1,TF_F2,TF_F3)
         ! Computes: e = ∇ • ( [U x B - j/σ] x B )
         implicit none
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: B,J,U
         type(VF),intent(in) :: sigmaInv_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: SF_CC
         type(VF),intent(inout) :: VF_F
         type(TF),intent(inout) :: TF_F1,TF_F2,TF_F3
         ! Compute uxB in E = j/sig - uxB
         call face2Face(TF_F1,U,m,SF_CC) ! U to tensor field
         call face2Face(TF_F2,B,m,SF_CC) ! B to tensor field
         call cross(VF_F,TF_F1,TF_F2) ! uxB
         call face2Face(TF_F1,VF_F,m,SF_CC) ! uxB to tensor field
         call edge2Face_no_diag(TF_F3,J,m) ! J to tensor field
         call multiply(TF_F3%x,sigmaInv_F) ! J/sig
         call multiply(TF_F3%y,sigmaInv_F) ! J/sig
         call multiply(TF_F3%z,sigmaInv_F) ! J/sig
         call subtract(TF_F3,TF_F1) ! E = j/sig - uxB
         call cross(VF_F,TF_F3,TF_F2) ! F = E x B
         call div(e,VF_F,m)
       end subroutine
       subroutine Poynting_export(e_integral,e,B,J,U,sigmaInv_F,m,SF_CC,VF_F,TF_F1,TF_F2,TF_F3,DT)
         implicit none
         real(cp),intent(inout) :: e_integral
         type(SF),intent(inout) :: e
         type(VF),intent(in) :: B,J,U
         type(VF),intent(in) :: sigmaInv_F
         type(mesh),intent(in) :: m
         type(SF),intent(inout) :: SF_CC
         type(VF),intent(inout) :: VF_F
         type(TF),intent(inout) :: TF_F1,TF_F2,TF_F3
         type(dir_tree),intent(in) :: DT
         call Poynting(e,B,J,U,sigmaInv_F,m,SF_CC,VF_F,TF_F1,TF_F2,TF_F3)
         call export_raw      (m,e,str(DT%e_budget_C),'Poynting',0)
         call export_processed(m,e,str(DT%e_budget_N),'Poynting',1)
         call Ln(e_integral,e,1.0_cp,m)
       end subroutine

       end module