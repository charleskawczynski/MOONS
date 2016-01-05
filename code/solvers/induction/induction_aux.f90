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

       implicit none

       private
       public :: compute_AddJCrossB
       public :: compute_JCrossB
       public :: compute_divBJ
       public :: compute_J
       public :: compute_TME_Fluid
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

       subroutine compute_AddJCrossB(jcrossB,B,B0,J_cc,m,D_fluid,Ha,Re,Rem,finite_Rem,temp,Bstar,temp_CC,jCrossB_F)
         ! addJCrossB computes the ith component of Ha^2/Re j x B
         ! where j is the total current and B is the applied or total mangetic
         ! field, depending on the solveBMethod.
         implicit none
         type(VF),intent(inout) :: jcrossB,temp
         type(VF),intent(in) :: B,B0
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid
         real(cp),intent(in) :: Ha,Re,Rem
         logical,intent(in) :: finite_Rem
         type(VF),intent(inout) :: J_cc,Bstar,temp_CC,jCrossB_F
         ! call zeroGhostPoints(temp)
         ! call zeroWall(temp,m)
         call assign(temp,0.0_cp)
         call compute_JCrossB(temp,B,B0,J_cc,m,D_fluid,Ha,Re,Rem,finite_Rem,Bstar,temp_CC,jCrossB_F)
         call add(jcrossB,temp)
       end subroutine

       subroutine compute_JCrossB(jcrossB,B,B0,J_cc,m,D_fluid,Ha,Re,Rem,finite_Rem,Bstar,temp_CC,jCrossB_F)
         ! computes
         ! 
         !     finite Rem:  Ha^2/(Re x Rem) curl(B_induced) x (B0 + B_induced)
         !     low Rem:     Ha^2/(Re)       curl(B_induced) x (B0)
         ! 
         implicit none
         type(VF),intent(inout) :: jcrossB
         type(VF),intent(in) :: B,B0
         type(mesh),intent(in) :: m
         type(domain),intent(in) :: D_fluid
         real(cp),intent(in) :: Ha,Re,Rem
         logical,intent(in) :: finite_Rem
         type(VF),intent(inout) :: J_cc,Bstar,temp_CC,jCrossB_F
         if (finite_Rem) then
           call add(Bstar,B,B0)
           call curl(J_cc,B,m)
           call cross(temp_CC,J_cc,Bstar)
           call cellCenter2Face(jCrossB_F,temp_CC,m)
           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         else
           call curl(J_cc,B,m)
           call cross(temp_CC,J_cc,B0)
           call cellCenter2Face(jCrossB_F,temp_CC,m)
           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         endif
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
         if (finite_Rem) call divide(J,Rem)
       end subroutine

       subroutine compute_TME_Fluid(KB_energy,B,nstep,compute_ME,D_fluid)
         implicit none
         type(probe),intent(inout) :: KB_energy
         type(VF),intent(in) :: B
         integer,intent(in) :: nstep
         logical,intent(in) :: compute_ME
         type(domain),intent(in) :: D_fluid
         real(cp) :: K_energy
         if (compute_ME) then
          call totalEnergy(K_energy,B,D_fluid)
          call set(KB_energy,nstep,K_energy)
          call apply(KB_energy)
         endif
       end subroutine

       subroutine compute_TME(KB_energy,B,nstep,compute_ME,m)
         implicit none
         type(probe),intent(inout) :: KB_energy
         type(VF),intent(in) :: B
         type(mesh),intent(in) :: m
         integer,intent(in) :: nstep
         logical,intent(in) :: compute_ME
         real(cp) :: K_energy
         if (compute_ME) then
          call totalEnergy(K_energy,B,m)
          call set(KB_energy,nstep,K_energy)
          call apply(KB_energy)
         endif
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