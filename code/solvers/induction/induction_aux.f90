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
       public :: compute_divB
       public :: compute_J
       public :: compute_KB_Fluid
       public :: compute_KB
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
         call zeroGhostPoints(temp)
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
           ! Magnetic Pressure (not yet done)
           call add(Bstar,B,B0)
           call square(Bstar)
           call multiply(Bstar,0.5_cp)
           call grad(jCrossB_F,Bstar,m)

           ! Magnetic Stress (not yet done)

           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/(Re*Rem))
         else

           call extractFace(jcrossB,jCrossB_F,D_fluid)
           call zeroGhostPoints(jCrossB)
           call multiply(jcrossB,Ha**2.0_cp/Re)
         endif
       end subroutine

       subroutine compute_divB(divB,divJ,B,J,m)
         implicit none
         type(VF),intent(inout) :: divB,divJ
         type(VF),intent(in) :: B,J
         type(mesh),intent(in) :: m
         call div(divB,B,m)
         call div(divJ,J,m)
       end subroutine

       subroutine compute_J(J,B,Rem,m,temp_B,finite_Rem)
         implicit none
         type(VF),intent(in) :: B,B0
         type(VF),intent(inout) :: J,temp_B
         real(cp),intent(in) :: Rem
         type(mesh),intent(in) :: m
         logical,intent(in) :: finite_Rem
         call add(temp_B,B0,B)
         call curl(J,temp_B,m)
         if (finite_Rem) call divide(J,Rem)
       end subroutine

       subroutine compute_KB_Fluid(B,B0,Bstar,KB_energy,KBi_energy,KB0_energy,D_fluid)
         implicit none
         type(VF),intent(in) :: B,B0
         type(VF),intent(inout) :: Bstar
         type(probe),intent(inout) :: KB_energy,KBi_energy,KB0_energy
         type(domain),intent(in) :: D_fluid
         real(cp) :: K_energy
         call add(Bstar,B0,B)
         call totalEnergy(K_energy,Bstar,D_fluid)
         call set(KB_f_energy,nstep,K_energy)
         call apply(KB_f_energy)

         call totalEnergy(K_energy,B,D_fluid)
         call set(KBi_f_energy,nstep,K_energy)
         call apply(KBi_f_energy)

         call totalEnergy(K_energy,B0,D_fluid)
         call set(KB0_f_energy,nstep,K_energy)
         call apply(KB0_f_energy)
       end subroutine

       subroutine compute_KB(B,B0,Bstar,KB_energy,KBi_energy,KB0_energy)
         implicit none
         type(VF),intent(in) :: B,B0
         type(VF),intent(inout) :: Bstar
         type(probe),intent(inout) :: KB_energy,KBi_energy,KB0_energy
         real(cp) :: K_energy
         call assign(Bstar,B)
         call add(Bstar,B0)
         call totalEnergy(K_energy,Bstar,m)
         call set(KB_energy,nstep,K_energy)
         call apply(KB_energy)

         call totalEnergy(K_energy,B,m)
         call set(KBi_energy,nstep,K_energy)
         call apply(KBi_energy)

         call totalEnergy(K_energy,B0,m)
         call set(KB0_energy,nstep,K_energy)
         call apply(KB0_energy)
       end subroutine

       subroutine embedVelocity_E(ind,U_E)
         implicit none
         type(induction),intent(inout) :: ind
         type(TF),intent(in) :: U_E ! Momentum edge velocity
         call embedEdge(U_E%x,U_E%x,D_fluid)
         call embedEdge(U_E%y,U_E%y,D_fluid)
         call embedEdge(U_E%z,U_E%z,D_fluid)
       end subroutine

       subroutine embedVelocity_F(ind,U_F)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U_F ! Momentum edge velocity
         call embedFace(U_Ft,U_F,D_fluid)
       end subroutine

       subroutine embedVelocity_CC(ind,U_CC)
         implicit none
         type(induction),intent(inout) :: ind
         type(VF),intent(in) :: U_CC ! Momentum edge velocity
         call embedCC(U_cct,U_CC,D_fluid)
       end subroutine

       end module