       module induction_aux_mod
       use current_precision_mod
       use GF_mod
       use SF_extend_mod
       use VF_extend_mod
       use TF_extend_mod
       use ops_embedExtract_mod
       use time_marching_params_mod
       use mesh_domain_extend_mod
       use mesh_extend_mod
       use apply_BCs_mod
       use export_raw_processed_mod
       use IO_export_mod
       use norms_extend_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use probe_mod
       use probe_extend_mod
       use ops_norms_mod
       use dimensionless_params_mod
       use data_location_extend_mod

       implicit none

       private
       public :: compute_divBJ
       public :: compute_J
       public :: compute_Total_Energy_Domain
       public :: compute_Total_Energy
       public :: embedVelocity_E
       public :: embedVelocity_F
       public :: embedVelocity_CC
       public :: set_sigma_inv_SF
       public :: set_sigma_inv_VF
       public :: set_insulating_above_lid

       contains

       subroutine compute_divBJ(divB,divJ,B,J,m)
         implicit none
         type(SF),intent(inout) :: divB,divJ
         type(VF),intent(in) :: B,J
         type(mesh),intent(in) :: m
         call div(divB,B,m)
         call div(divJ,J,m)
       end subroutine

       subroutine compute_J(J,B,scale,m)
         implicit none
         type(VF),intent(inout) :: B
         type(VF),intent(inout) :: J
         real(cp),intent(in) :: scale
         type(mesh),intent(in) :: m
         call curl(J,B,m)
         call multiply(J,scale)
       end subroutine

       subroutine compute_Total_Energy_Domain(energy,field,field_domain,TMP,m,scale,MD)
         implicit none
         type(probe),intent(inout) :: energy
         type(VF),intent(in) :: field
         type(VF),intent(inout) :: field_domain
         type(time_marching_params),intent(in) :: TMP
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         real(cp),intent(in) :: scale
         real(cp) :: temp
         call extractCC(field_domain,field,MD)
         call assign_ghost_XPeriodic(field_domain,0.0_cp)
         call compute_Ln(temp,field_domain,2.0_cp,m,MD)
         temp = scale*0.5_cp*temp
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
         call compute_Ln(temp,field,2.0_cp,m)
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
         type(mesh) :: m_other
         call init_other(m_other,m_ind,MD_sigma)
         call init(sigma_inv_temp,m_other,get_DL(sigma_inv))
         call assign(sigma_inv_temp,1.0_cp)
         call assign(sigma_inv,1.0_cp/DP%sig_local_over_sig_f)
         call embed(sigma_inv,sigma_inv_temp,MD_sigma)
         call delete(sigma_inv_temp)
         call delete(m_other)
       end subroutine

       subroutine set_sigma_inv_VF(sigma_inv,m_ind,MD_sigma,DP)
         implicit none
         type(VF),intent(inout) :: sigma_inv
         type(mesh),intent(in) :: m_ind
         type(mesh_domain),intent(in) :: MD_sigma
         type(dimensionless_params),intent(in) :: DP
         type(VF) :: sigma_inv_temp
         type(mesh) :: m_other
         call init_other(m_other,m_ind,MD_sigma)
         call init(sigma_inv_temp,m_other,get_DL(sigma_inv))
         call assign(sigma_inv_temp,1.0_cp)
         call assign(sigma_inv,1.0_cp/DP%sig_local_over_sig_f)
         ! call set_insulating_above_lid(sigma_inv_temp,m_other,1.0_cp/DP%sig_local_over_sig_f,1.0_cp)
         call embed(sigma_inv,sigma_inv_temp,MD_sigma)
         call delete(sigma_inv_temp)
         call delete(m_other)
       end subroutine

       subroutine set_insulating_above_lid(sigma_inv,m,sigma_inv_lid,y_lid)
         implicit none
         type(VF),intent(inout) :: sigma_inv
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: y_lid,sigma_inv_lid
         integer,dimension(3) :: s
         logical,dimension(4) :: L
         integer :: j,t
         real(cp) :: tol
         tol = m%MP%dhmin_min*10.0_cp**(-3.0_cp)
         do t=1,m%s
         s = sigma_inv%x%BF(t)%GF%s
         do j=1,s(2)
         L(1) =  N_along(sigma_inv%x%DL,2)
         L(2) = CC_along(sigma_inv%x%DL,2)
         if (L(1)) L(3) = (m%B(t)%g%c(2)%hn%f(j).gt.y_lid-tol)
         if (L(2)) L(4) = (m%B(t)%g%c(2)%hc%f(j).gt.y_lid-tol)
         if ((L(1).and.L(3)).or.(L(2).and.L(4))) call assign_plane(sigma_inv%x%BF(t)%GF,sigma_inv_lid,j,2)
         enddo
         s = sigma_inv%y%BF(t)%GF%s
         do j=1,s(2)
         L(1) =  N_along(sigma_inv%y%DL,2)
         L(2) = CC_along(sigma_inv%y%DL,2)
         if (L(1)) L(3) = (m%B(t)%g%c(2)%hn%f(j).gt.y_lid-tol)
         if (L(2)) L(4) = (m%B(t)%g%c(2)%hc%f(j).gt.y_lid-tol)
         if ((L(1).and.L(3)).or.(L(2).and.L(4))) call assign_plane(sigma_inv%y%BF(t)%GF,sigma_inv_lid,j,2)
         enddo
         s = sigma_inv%z%BF(t)%GF%s
         do j=1,s(2)
         L(1) =  N_along(sigma_inv%z%DL,2)
         L(2) = CC_along(sigma_inv%z%DL,2)
         if (L(1)) L(3) = (m%B(t)%g%c(2)%hn%f(j).gt.y_lid-tol)
         if (L(2)) L(4) = (m%B(t)%g%c(2)%hc%f(j).gt.y_lid-tol)
         if ((L(1).and.L(3)).or.(L(2).and.L(4))) call assign_plane(sigma_inv%z%BF(t)%GF,sigma_inv_lid,j,2)
         enddo
         enddo
       end subroutine

       end module