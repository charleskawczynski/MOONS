       module E_K_budget_mod
       use current_precision_mod
       use string_mod
       use path_extend_mod
       use dir_tree_mod
       use mesh_extend_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use IO_tools_mod
       use E_K_budget_terms_mod
       use mesh_domain_extend_mod
       use ops_discrete_mod
       use ops_interp_mod
       use ops_embedExtract_mod
       use ops_mirror_field_mod
       use time_marching_params_mod
       use dimensionless_params_mod
       use mirror_props_extend_mod
       implicit none

       private
       public :: E_K_Budget_wrapper
       public :: E_K_Budget_no_dummies
       public :: compute_E_K_budget_fields
       public :: export_integral_E_K_budget

       contains

       subroutine E_K_Budget_wrapper(DT,U,Unm1,B,B0,J,p,m,TMP,DP,MP,MD_fluid)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: U,Unm1,B,B0,J
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD_fluid
         type(dimensionless_params),intent(in) :: DP
         type(mirror_props),intent(in) :: MP
         type(time_marching_params),intent(in) :: TMP
         type(mesh_domain) :: MD_fluid_temp,temp
         type(mesh) :: m_temp
         type(VF) :: U_temp,Unm1_temp,B_temp,B0_temp,J_temp
         type(SF) :: p_temp
         if (MP%mirror) then
          call mirror_field(m_temp,U_temp,m,U,MP)
          call mirror_field(m_temp,Unm1_temp,m,Unm1,MP)
          call mirror_field(m_temp,p_temp,m,p,MP)
          call mirror_field(m_temp,B_temp,m,B,anti_mirror(MP))
          call mirror_field(m_temp,B0_temp,m,B0,MP)
          call mirror_field(m_temp,J_temp,m,J,MP)
          call mirror_mesh(temp%m_R1,MD_fluid%m_R1,MP)
          call mirror_mesh(temp%m_R2,MD_fluid%m_R2,MP)
          call init(MD_fluid_temp,temp%m_R1,temp%m_R1)
          call E_K_Budget_no_dummies(DT,U_temp,Unm1_temp,B_temp,B0_temp,&
          J_temp,p_temp,m_temp,TMP,DP,MD_fluid_temp)
          call delete(MD_fluid_temp)
          call delete(temp)
          call delete(m_temp)
          call delete(U_temp)
          call delete(Unm1_temp)
          call delete(p_temp)
          call delete(B_temp)
          call delete(B0_temp)
          call delete(J_temp)
         else
          call E_K_Budget_no_dummies(DT,U,Unm1,B,B0,J,p,m,TMP,DP,MD_fluid)
         endif
       end subroutine

       subroutine E_K_Budget_no_dummies(DT,U,Unm1,B,B0,J,p,m,TMP,DP,MD_fluid)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: U,Unm1,B,B0,J
         type(SF),intent(in) :: p
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD_fluid
         type(dimensionless_params),intent(in) :: DP
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(8) :: e_budget
         type(SF) :: e
         type(VF) :: VF_F1,VF_F2,temp_B,temp_B0,temp_J,U_CC
         type(TF) :: TF_CC1,TF_CC2
         call init_CC(e,m)
         call init_CC(U_CC,m)
         call face2CellCenter(U_CC,U,m)

         call init_CC(TF_CC1,m)
         call init_CC(TF_CC2,m)
         call init_Face(VF_F1,m)
         call init_Face(VF_F2,m)

         call init_Face(temp_B,m)
         call init_Face(temp_B0,m)
         call init_Edge(temp_J,m)

         call extractFace(temp_B,B,MD_fluid)
         call extractFace(temp_B0,B0,MD_fluid)
         call extractEdge(temp_J,J,MD_fluid)

         call compute_E_K_budget_fields(DT,e_budget,U,Unm1,U_CC,&
         temp_B,temp_B0,temp_J,p,m,TMP%dt,DP,&
         VF_F1,VF_F2,TF_CC1,TF_CC2)

         call export_integral_E_K_budget(DT,TMP,e_budget)

         call delete(e)
         call delete(VF_F1)
         call delete(VF_F2)
         call delete(temp_B)
         call delete(temp_B0)
         call delete(temp_J)
         call delete(U_CC)
         call delete(TF_CC1)
         call delete(TF_CC2)
       end subroutine

       subroutine compute_E_K_budget_fields(DT,e_int,U,Unm1,U_CC,B,B0,J,p,m,dTime,DP,&
         VF_F1,VF_F2,TF_CC1,TF_CC2)
         implicit none
         type(dir_tree),intent(in) :: DT
         type(VF),intent(in) :: U,Unm1,U_CC,B,B0,J
         type(SF),intent(in) :: p
         type(VF),intent(inout) :: VF_F1,VF_F2
         type(TF),intent(inout) :: TF_CC1,TF_CC2
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: dTime
         type(dimensionless_params),intent(in) :: DP
         real(cp),dimension(8),intent(inout) :: e_int
         type(SF) :: e
         real(cp) :: scale,Al,Re_inv
         integer :: i
         call init(e,TF_CC1%x%x)
         i = 1
         Al = DP%Al
         Re_inv = 1.0_cp/DP%Re
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

       subroutine export_integral_E_K_budget(DT,TMP,e_budget)
         implicit none
         type(time_marching_params),intent(in) :: TMP
         real(cp),dimension(8),intent(in) :: e_budget
         type(dir_tree),intent(in) :: DT
         type(string),dimension(8) :: vars
         integer :: un,i
         un = new_and_open(str(DT%e_budget),'E_K_budget_terms')
         i=1
         call init(vars(i),'Unsteady = '); i=i+1
         call init(vars(i),'E_K_Convection = '); i=i+1
         call init(vars(i),'E_K_Diffusion = '); i=i+1
         call init(vars(i),'E_K_Pressure = '); i=i+1
         call init(vars(i),'Viscous_Dissipation = '); i=i+1
         call init(vars(i),'E_M_Convection = '); i=i+1
         call init(vars(i),'E_M_Tension = '); i=i+1
         call init(vars(i),'Lorentz = '); i=i+1

         write(un,*) 'kinetic energy budget at t=',TMP%t
         do i=1,size(vars)
         write(un,*) str(vars(i)),e_budget(i)
         call delete(vars(i))
         enddo
         flush(un)
         call close_and_message(un,str(DT%e_budget),'E_K_budget_terms')
       end subroutine

       end module