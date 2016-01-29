       module induction_aux_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use ops_embedExtract_mod
       use domain_mod
       use mesh_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use probe_base_mod
       use probe_transient_mod
       use ops_norms_mod

       implicit none

       private
       public :: compute_AddJCrossB
       public :: compute_JCrossB
       public :: compute_divBJ
       public :: compute_J
       public :: compute_TME_Domain
       public :: compute_TME
       public :: embedVelocity_E
       public :: embedVelocity_F
       public :: embedVelocity_CC

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

       subroutine compute_AddJCrossB(jcrossB,B,B0,J,m,D_fluid,Ha,Re,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF,temp)
         implicit none
         type(VF),intent(inout) :: jcrossB,temp
         type(VF),intent(in) :: B,B0
         type(VF),intent(in) :: J
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid
         real(cp),intent(in) :: Ha,Re
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(VF),intent(inout) :: temp_F
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call compute_JCrossB(temp,B,B0,J,m,D_fluid,Ha,Re,finite_Rem,&
         temp_CC,temp_F,temp_F1_TF,temp_F2_TF)
         call add(jcrossB,temp)
       end subroutine

       subroutine compute_JCrossB(jCrossB,B,B0,J,m,D_fluid,Ha,Re,finite_Rem,&
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
         type(domain),intent(in) :: D_fluid
         real(cp),intent(in) :: Ha,Re
         logical,intent(in) :: finite_Rem
         type(SF),intent(inout) :: temp_CC
         type(TF),intent(inout) :: temp_F1_TF,temp_F2_TF
         call edge2Face_no_diag(temp_F1_TF,J,m)
         if (finite_Rem) then; call add(temp_F,B0,B); call face2Face_no_diag(temp_F2_TF,temp_F,m,temp_CC)
         else;                                        call face2Face_no_diag(temp_F2_TF,B0    ,m,temp_CC)
         endif
         call cross(temp_F,temp_F1_TF,temp_F2_TF)
         call extractFace(jCrossB,temp_F,D_fluid)
         call zeroGhostPoints(jCrossB)
         call multiply(jCrossB,Ha**2.0_cp/Re)
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
         type(VF),intent(in) :: B
         type(VF),intent(inout) :: J
         real(cp),intent(in) :: Rem
         type(mesh),intent(in) :: m
         logical,intent(in) :: finite_Rem
         call curl(J,B,m)
         if (finite_Rem) call multiply(J,1.0_cp/Rem)
       end subroutine

       subroutine compute_TME_Domain(K_energy,KB_energy,B,nstep,D)
         implicit none
         real(cp),intent(inout) :: K_energy
         type(probe),intent(inout) :: KB_energy
         type(VF),intent(in) :: B
         integer,intent(in) :: nstep
         type(domain),intent(in) :: D
         type(VF) :: temp
         call init_CC(temp,D%m_in)
         call extractCC(temp,B,D)
         call Ln(K_energy,temp,2.0_cp,D%m_in)
         call delete(temp)
         call set(KB_energy,nstep,K_energy)
         call apply(KB_energy)
       end subroutine

       subroutine compute_TME(K_energy,KB_energy,B,nstep,m)
         implicit none
         real(cp),intent(inout) :: K_energy
         type(probe),intent(inout) :: KB_energy
         type(VF),intent(in) :: B
         integer,intent(in) :: nstep
         type(mesh),intent(in) :: m
         call Ln(K_energy,B,2.0_cp,m)
         call set(KB_energy,nstep,K_energy)
         call apply(KB_energy)
       end subroutine

       subroutine embedVelocity_E(U_E_tot,U_E_in,D_fluid)
         implicit none
         type(TF),intent(inout) :: U_E_tot
         type(TF),intent(in) :: U_E_in ! Momentum edge velocity
         type(domain),intent(in) :: D_fluid
         call embedEdge(U_E_tot%x,U_E_in%x,D_fluid)
         call embedEdge(U_E_tot%y,U_E_in%y,D_fluid)
         call embedEdge(U_E_tot%z,U_E_in%z,D_fluid)
       end subroutine

       subroutine embedVelocity_F(U_Ft,U_F,D_fluid)
         implicit none
         type(VF),intent(inout) :: U_Ft
         type(VF),intent(in) :: U_F ! Momentum edge velocity
         type(domain),intent(in) :: D_fluid
         call embedFace(U_Ft,U_F,D_fluid)
       end subroutine

       subroutine embedVelocity_CC(U_cct,U_CC,D_fluid)
         implicit none
         type(VF),intent(inout) :: U_cct
         type(VF),intent(in) :: U_CC ! Momentum edge velocity
         type(domain),intent(in) :: D_fluid
         call embedCC(U_cct,U_CC,D_fluid)
       end subroutine

       end module