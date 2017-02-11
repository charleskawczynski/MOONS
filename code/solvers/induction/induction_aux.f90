       module induction_aux_mod
       use current_precision_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_embedExtract_mod
       use time_marching_params_mod
       use mesh_domain_mod
       use mesh_mod
       use apply_BCs_mod
       use export_raw_processed_mod
       use IO_export_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use probe_mod
       use ops_norms_mod
       use dimensionless_params_mod
       use data_location_mod

       implicit none

       private
       public :: compute_AddJCrossB
       public :: compute_JCrossB
       public :: compute_Add_MPG
       public :: compute_add_Q2D_JCrossB
       public :: compute_Q2D_JCrossB
       public :: compute_divBJ
       public :: compute_J
       public :: compute_Total_Energy_Domain
       public :: compute_Total_Energy
       public :: embedVelocity_E
       public :: embedVelocity_F
       public :: embedVelocity_CC
       public :: set_sigma_inv_SF
       public :: set_sigma_inv_VF

       contains

       subroutine compute_AddJCrossB(jcrossB,B,B0,J,m,D_fluid,N,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF,temp)
         implicit none
         type(VF),intent(inout) :: jcrossB,temp
         type(VF),intent(in) :: B,B0
         type(VF),intent(in) :: J
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: D_fluid
         real(cp),intent(in) :: N
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(VF),intent(inout) :: temp_F
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call compute_JCrossB(temp,B,B0,J,m,D_fluid,N,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF)
         call add(jcrossB,temp)
       end subroutine

       subroutine compute_JCrossB(jCrossB,B,B0,J,m,D_fluid,N,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF)
         ! computes
         !
         !     finite Rem:  Ha^2/Re J x (B0 + B_induced)
         !     low    Rem:  Ha^2/Re J x (B0)
         !
         implicit none
         type(VF),intent(inout) :: jCrossB,temp_F
         type(VF),intent(in) :: B,B0,J
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: D_fluid
         real(cp),intent(in) :: N
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call edge2Face_no_diag(temp_F1_TF,J,m)
         if (finite_Rem) then; call add(temp_F,B0,B); call face2Face_no_diag(temp_F2_TF,temp_F,m,temp_CC)
         else;                                        call face2Face_no_diag(temp_F2_TF,B0    ,m,temp_CC)
         endif
         call cross_product(temp_F,temp_F1_TF,temp_F2_TF)
         call extractFace(jCrossB,temp_F,D_fluid)
         call assign_ghost_XPeriodic(jCrossB,0.0_cp)
         call multiply(jCrossB,N)
       end subroutine

       subroutine compute_Add_MPG(U,TMP,dir)
         type(VF),intent(inout) :: U
         type(time_marching_params),intent(in) :: TMP
         integer,intent(in) :: dir
         select case (dir)
         case (0);
         case (1); call subtract(U%x,-TMP%dt*1.0_cp); call apply_BCs(U)
         case (2); call subtract(U%y,-TMP%dt*1.0_cp); call apply_BCs(U)
         case (3); call subtract(U%z,-TMP%dt*1.0_cp); call apply_BCs(U)
         case default; stop 'Error: dir must = 0:3 in compute_MPG in induction_aux.f90'
         end select
       end subroutine

       subroutine compute_add_Q2D_JCrossB(Q2D_JCrossB,U,tau,temp_F)
         implicit none
         type(VF),intent(inout) :: Q2D_JCrossB,temp_F
         type(VF),intent(in) :: U
         real(cp),intent(in) :: tau
         call compute_Q2D_JCrossB(temp_F,U,tau)
         call add(Q2D_JCrossB,temp_F)
       end subroutine

       subroutine compute_Q2D_JCrossB(Q2D_JCrossB,U,tau)
         ! computes: Q2D_JCrossB = -U/tau, tau = Re/Ha
         implicit none
         type(VF),intent(inout) :: Q2D_JCrossB
         type(VF),intent(in) :: U
         real(cp),intent(in) :: tau
         call assign(Q2D_JCrossB,U)
         call multiply(Q2D_JCrossB,-1.0_cp/tau)
       end subroutine

       subroutine compute_divBJ(divB,divJ,B,J,m)
         implicit none
         type(SF),intent(inout) :: divB,divJ
         type(VF),intent(in) :: B,J
         type(mesh),intent(in) :: m
         call div(divB,B,m)
         call div(divJ,J,m)
       end subroutine

       subroutine compute_J(J,B,Rem,m,finite_Rem)
         implicit none
         type(VF),intent(inout) :: B
         type(VF),intent(inout) :: J
         real(cp),intent(in) :: Rem
         type(mesh),intent(in) :: m
         logical,intent(in) :: finite_Rem
         call curl(J,B,m)
         if (finite_Rem) call multiply(J,1.0_cp/Rem)
       end subroutine

       subroutine compute_Total_Energy_Domain(energy,field,TMP,m,scale,MD)
         implicit none
         type(probe),intent(inout) :: energy
         type(VF),intent(in) :: field
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         real(cp),intent(in) :: scale
         type(VF) :: temp_VF
         real(cp) :: temp
         call init_CC(temp_VF,m,MD)
         call extractCC(temp_VF,field,MD)
         call assign_ghost_XPeriodic(temp_VF,0.0_cp)
         call Ln(temp,temp_VF,2.0_cp,m,MD)
         temp = scale*0.5_cp*temp
         call delete(temp_VF)
         call export(energy,TMP,temp)
       end subroutine

       subroutine compute_Total_Energy(energy,field,TMP,m,scale)
         implicit none
         type(probe),intent(inout) :: energy
         type(VF),intent(inout) :: field
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         real(cp) :: temp
         call assign_ghost_XPeriodic(field,0.0_cp) ! norms now includes ghost points
         call Ln(temp,field,2.0_cp,m)
         temp = scale*0.5_cp*temp
         call export(energy,TMP,temp)
       end subroutine

       subroutine embedVelocity_E(U_E_tot,U_E_in,D_fluid)
         implicit none
         type(TF),intent(inout) :: U_E_tot
         type(TF),intent(in) :: U_E_in ! Momentum edge velocity
         type(mesh_domain),intent(in) :: D_fluid
         call embedEdge(U_E_tot%x,U_E_in%x,D_fluid)
         call embedEdge(U_E_tot%y,U_E_in%y,D_fluid)
         call embedEdge(U_E_tot%z,U_E_in%z,D_fluid)
       end subroutine

       subroutine embedVelocity_F(U_Ft,U_F,D_fluid)
         implicit none
         type(VF),intent(inout) :: U_Ft
         type(VF),intent(in) :: U_F ! Momentum edge velocity
         type(mesh_domain),intent(in) :: D_fluid
         call embedFace(U_Ft,U_F,D_fluid)
       end subroutine

       subroutine embedVelocity_CC(U_cct,U_CC,D_fluid)
         implicit none
         type(VF),intent(inout) :: U_cct
         type(VF),intent(in) :: U_CC ! Momentum edge velocity
         type(mesh_domain),intent(in) :: D_fluid
         call embedCC(U_cct,U_CC,D_fluid)
       end subroutine

       subroutine set_sigma_inv_SF(sigma_inv,m_ind,MD_sigma,DP)
         implicit none
         type(SF),intent(inout) :: sigma_inv
         type(mesh),intent(in) :: m_ind
         type(mesh_domain),intent(in) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(SF) :: sigma_inv_temp
         call init(sigma_inv_temp,m_ind,get_DL(sigma_inv))
         call assign(sigma_inv_temp,1.0_cp)
         call assign(sigma_inv,1.0_cp/DP%sig_local_over_sig_f)
         call embed(sigma_inv,sigma_inv_temp,MD_sigma)
         call delete(sigma_inv_temp)
       end subroutine

       subroutine set_sigma_inv_VF(sigma_inv,m_ind,MD_sigma,DP)
         implicit none
         type(VF),intent(inout) :: sigma_inv
         type(mesh),intent(in) :: m_ind
         type(mesh_domain),intent(in) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(VF) :: sigma_inv_temp
         call init(sigma_inv_temp,m_ind,get_DL(sigma_inv))
         call assign(sigma_inv_temp,1.0_cp)
         call assign(sigma_inv,1.0_cp/DP%sig_local_over_sig_f)
         call embed(sigma_inv,sigma_inv_temp,MD_sigma)
         call delete(sigma_inv_temp)
       end subroutine

       end module