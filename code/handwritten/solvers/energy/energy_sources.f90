       module energy_sources_mod
       use current_precision_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       use TF_mod
       use norms_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod
       use apply_BCs_mod
       use PCG_mod
       use PCG_solver_mod

       implicit none

       private
       public :: add_advection
       public :: add_diffusion
       public :: add_KE_diffusion
       public :: add_viscous_dissipation
       public :: add_joule_heating
       public :: add_volumetric_heating_Nuclear

       contains

       subroutine add_advection(S_CC,T,U_F,scale,m,temp_CC1,temp_F)
         ! Adds S_CC = S_CC + scale ∇•(uT)
         implicit none
         type(SF),intent(inout) :: S_CC,T,temp_CC1
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_F
         real(cp),intent(in) :: scale
         type(VF),intent(inout) :: temp_F
         call cellCenter2Face(temp_F,T,m)
         call multiply(temp_F,U_F)
         call div(temp_CC1,temp_F,m)
         call multiply(temp_CC1,-1.0_cp)
         call add_product(S_CC,temp_CC1,scale)
       end subroutine

       subroutine add_diffusion(S_CC,T,scale,m,temp_CC1)
         ! Adds S_CC = S_CC + scale ∇²T
         implicit none
         type(SF),intent(inout) :: S_CC,T,temp_CC1
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call lap(temp_CC1,T,m)
         call add_product(S_CC,temp_CC1,scale)
       end subroutine

       subroutine add_KE_diffusion(S_CC,U_CC,scale,m,temp_CC1,temp_CC2,temp_CC_VF)
         ! Adds S_CC = S_CC + scale J•J/sigma
         implicit none
         type(SF),intent(inout) :: S_CC,temp_CC1,temp_CC2
         type(VF),intent(inout) :: temp_CC_VF
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_CC
         real(cp),intent(in) :: scale
         call assign(temp_CC_VF,U_CC)
         call square(temp_CC_VF)
         call add(temp_CC1,temp_CC_VF)
         call lap(temp_CC2,temp_CC1,m)
         call add_product(S_CC,temp_CC2,scale)
       end subroutine

       subroutine add_viscous_dissipation(S_CC,U_CC,scale,m,temp_CC,temp_CC_TF)
         ! Adds S_CC = S_CC + scale (∇u)•(∇u)
         implicit none
         type(SF),intent(inout) :: S_CC,temp_CC
         type(mesh),intent(in) :: m
         type(VF),intent(in) :: U_CC
         TYPE(TF),intent(inout) :: temp_CC_TF
         real(cp),intent(in) :: scale
         call grad(temp_CC_TF,U_CC,m)
         call square(temp_CC_TF)
         call add(temp_CC,temp_CC_TF)
         call add_product(S_CC,temp_CC,scale)
       end subroutine

       subroutine add_joule_heating(S_CC,J,sigmaInv_CC,scale,m,temp_CC,temp_F,temp_CC_VF)
         ! Adds S_CC = S_CC + scale J•J/sigma
         implicit none
         type(SF),intent(inout) :: S_CC,temp_CC
         type(VF),intent(in) :: J
         type(SF),intent(in) :: sigmaInv_CC
         type(VF),intent(inout) :: temp_F,temp_CC_VF
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         call edge2CellCenter(temp_CC_VF,J,m,temp_F)
         call square(temp_CC_VF)
         call add(temp_CC,temp_CC_VF)
         call multiply(temp_CC,sigmaInv_CC)
         call add_product(S_CC,temp_CC,scale)
       end subroutine

       subroutine add_volumetric_heating_Nuclear(S_CC,m,scale,temp_CC)
         implicit none
         type(SF),intent(inout) :: S_CC,temp_CC
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: scale
         integer :: t,i,j,k
         real(cp) :: a,L,m_,F
         a = 1.0_cp
         L = 1.0_cp
         m_ = a/L
         F = scale*2.0_cp*m_/(1.0_cp-exp(-2.0_cp*m_))
         !$OMP PARALLEL DO
         do t=1,m%s; do k=1,temp_CC%BF(t)%GF%s(3); do j=1,temp_CC%BF(t)%GF%s(2); do i=1,temp_CC%BF(t)%GF%s(1)
         temp_CC%BF(t)%GF%f(i,j,k) = F*exp(-m_*(m%B(t)%g%c(2)%hc%f(j)+1.0_cp))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         call add_product(S_CC,temp_CC,scale)
       end subroutine

       end module