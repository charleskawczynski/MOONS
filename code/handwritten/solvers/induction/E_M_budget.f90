       module E_M_budget_mod
       use current_precision_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use IO_tools_mod
       use E_M_budget_terms_mod
       use mesh_domain_extend_mod
       use ops_discrete_mod
       use induction_aux_mod
       use ops_interp_mod
       use ops_embedExtract_mod
       use ops_mirror_field_mod
       use time_marching_params_mod
       use dimensionless_params_mod
       use mirror_props_mod
       use export_raw_processed_mod
       implicit none

       private
       public :: E_M_Budget_wrapper
       public :: E_M_Budget_no_dummies
       public :: compute_E_M_Budget_fields
       public :: export_integral_E_M_budget

       contains

       subroutine E_M_Budget_wrapper(DT,U,B,Bnm1,B0,B0nm1,J,m,MD_fluid,MD_sigma,DP,TMP,MP)
         implicit none
         type(VF),intent(in) :: U,B,Bnm1,B0,B0nm1,J
         type(mesh_domain),intent(in) :: MD_fluid,MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(time_marching_params),intent(in) :: TMP
         type(dir_tree),intent(in) :: DT
         type(mesh),intent(in) :: m
         type(mirror_props),intent(in) :: MP
         type(mesh_domain) :: MD_fluid_temp,MD_sigma_temp,temp
         type(mesh) :: m_temp
         type(VF) :: U_temp,B_temp,Bnm1_temp,B0_temp,B0nm1_temp,J_temp
         if (MP%mirror) then
          call mirror_field(m_temp,U_temp,m,U,MP)
          call mirror_field(m_temp,B_temp,m,B,MP)
          call mirror_field(m_temp,Bnm1_temp,m,Bnm1,MP)
          call mirror_field(m_temp,B0_temp,m,B0,MP)
          call mirror_field(m_temp,B0nm1_temp,m,B0nm1,MP)
          call mirror_field(m_temp,J_temp,m,J,MP)

          call mirror_mesh(temp%m_R1,MD_fluid%m_R1,MP)
          call mirror_mesh(temp%m_R2,MD_fluid%m_R2,MP)
          call init(MD_fluid_temp,temp%m_R1,temp%m_R2)

          call mirror_mesh(temp%m_R1,MD_sigma%m_R1,MP)
          call mirror_mesh(temp%m_R2,MD_sigma%m_R2,MP)
          call init(MD_sigma_temp,temp%m_R1,temp%m_R2)

          call E_M_Budget_no_dummies(DT,U_temp,B_temp,Bnm1_temp,B0_temp,B0nm1_temp,&
          J_temp,m_temp,MD_fluid_temp,MD_sigma_temp,DP,TMP)
          call delete(MD_fluid_temp)
          call delete(MD_sigma_temp)
          call delete(temp)
          call delete(m_temp)
          call delete(U_temp)
          call delete(B_temp)
          call delete(Bnm1_temp)
          call delete(B0_temp)
          call delete(B0nm1_temp)
          call delete(J_temp)
         else
          call E_M_Budget_no_dummies(DT,U,B,Bnm1,B0,B0nm1,J,m,MD_fluid,MD_sigma,DP,TMP)
         endif
       end subroutine

       subroutine E_M_Budget_no_dummies(DT,U,B,Bnm1,B0,B0nm1,J,m,MD_fluid,MD_sigma,DP,TMP)
         implicit none
         type(VF),intent(in) :: U,B,Bnm1,B0,B0nm1,J
         type(mesh_domain),intent(in) :: MD_fluid,MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(dir_tree),intent(in) :: DT
         real(cp),dimension(3) :: e_budget
         type(mesh) :: m_conduct
         type(SF) :: sigInv_CC,sigInv_CC_conduct,sigInv_CC_ideal
         type(VF) :: sigInv_F,sigInv_F_conduct,sigInv_F_ideal
         type(VF) :: temp_F1,temp_U,temp_CC_VF,B_temp,Bnm1_temp
         type(TF) :: temp_CC_TF,temp_F1_TF,temp_F2_TF,temp_F3_TF

         call init_other(m_conduct,m,MD_sigma)
         call init_CC(temp_CC_TF,m)
         call init_CC(temp_CC_VF,m)
         call init_Face(temp_F1,m)
         call init_Face(temp_F1_TF,m)
         call init_Face(temp_F2_TF,m)
         call init_Face(temp_F3_TF,m)
         call init_Face(temp_U,m)
         call init_CC(sigInv_CC_conduct,m_conduct)
         call init_CC(sigInv_CC_ideal,m)
         call init_CC(sigInv_CC,m)
         call init_Face(sigInv_F_conduct,m_conduct)
         call init_Face(sigInv_F_ideal,m)
         call init_Face(sigInv_F,m)

         call init(B_temp,B)
         call init(Bnm1_temp,Bnm1)
         call assign(B_temp,B)
         call assign(Bnm1_temp,Bnm1)
         call add(B_temp,B0)
         call add(Bnm1_temp,B0nm1)

         call set_sigma_inv_VF(sigInv_F,m,MD_sigma,DP)
         call set_sigma_inv_SF(sigInv_CC,m,MD_sigma,DP)

         call assign(     sigInv_F_ideal,   0.0_cp)
         call assign(     sigInv_CC_ideal,  0.0_cp)
         call extractFace(sigInv_F_conduct, sigInv_F,         MD_sigma)
         call extractCC(  sigInv_CC_conduct,sigInv_CC,        MD_sigma)
         call embedFace(  sigInv_F_ideal,   sigInv_F_conduct, MD_sigma)
         call embedCC(    sigInv_CC_ideal,  sigInv_CC_conduct,MD_sigma)

         call embedFace(temp_U,U,MD_fluid)

         call compute_E_M_Budget_fields(DT,e_budget,B_temp,Bnm1_temp,J,&
         sigInv_F_ideal,sigInv_CC_ideal,temp_U,m,TMP%TS%dt,DP,&
         temp_CC_TF,temp_CC_VF,temp_F1,&
         temp_F1_TF,temp_F2_TF,temp_F3_TF)

         call export_integral_E_M_budget(DT,TMP,e_budget)

         call delete(m_conduct)
         call delete(B_temp)
         call delete(Bnm1_temp)
         call delete(temp_CC_TF)
         call delete(temp_CC_VF)
         call delete(temp_F1)
         call delete(temp_F1_TF)
         call delete(temp_F2_TF)
         call delete(temp_F3_TF)
         call delete(temp_U)
         call delete(sigInv_CC_ideal)
         call delete(sigInv_CC_conduct)
         call delete(sigInv_F)
         call delete(sigInv_CC)
         call delete(sigInv_F_conduct)
         call delete(sigInv_F_ideal)
       end subroutine

       subroutine compute_E_M_Budget_fields(DT,e_int,B,Bnm1,J,sigInv_F,&
         sigInv_CC,U,m,dTime,DP,TF_CC,VF_CC,VF_F1,TF_F1,TF_F2,TF_F3)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: B,Bnm1,J,sigInv_F,U
         type(SF),intent(in) :: sigInv_CC
         type(VF),intent(inout) :: VF_F1,VF_CC
         type(TF),intent(inout) :: TF_CC,TF_F1,TF_F2,TF_F3
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime
         type(dimensionless_params),intent(in) :: DP
         real(cp),dimension(3),intent(inout) :: e_int
         integer :: i
         real(cp) :: scale,Al,N
         type(SF) :: e
         call init(e,sigInv_CC)
         i = 1
         Al = DP%Al
         N = DP%N
         scale=1.0_cp;call export_Unsteady(e_int(i),e,B,Bnm1,dTime,m,scale,TF_CC%x,TF_CC%y,DT); i=i+1
         scale=N; call export_Joule_Heat(e_int(i),e,J,sigInv_CC,m,scale,TF_CC%x,VF_F1,DT); i=i+1
         scale=Al;call export_Poynting(e_int(i),e,B,J,U,sigInv_F,m,scale,VF_CC%x,VF_F1,TF_F1,TF_F2,TF_F3,DT); i=i+1
         call delete(e)
       end subroutine

       subroutine export_integral_E_M_budget(DT,TMP,e_budget)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(3),intent(in) :: e_budget
         type(dir_tree),intent(in) :: DT
         type(string),dimension(3) :: vars
         integer :: un,i
         un = new_and_open(str(DT%e_budget),'E_M_budget_terms')
         call init(vars(1),'Unsteady = ')
         call init(vars(2),'Joule_Heat = ')
         call init(vars(3),'Poynting = ')

         write(un,*) 'magnetic energy budget at t=',TMP%t
         do i=1,3
         write(un,*) str(vars(i)),e_budget(i)
         call delete(vars(i))
         enddo
         call close_and_message(un,str(DT%e_budget),'E_M_budget_terms')
       end subroutine

       end module