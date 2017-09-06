       module energy_aux_mod
       use current_precision_mod
       use SF_extend_mod
       use VF_mod

       use ops_embedExtract_mod
       use mesh_domain_mod
       use mesh_extend_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none

       private
       public :: compute_Q
       public :: volumetric_heating_intro
       public :: volumetric_heating_equation
       public :: compute_divQ

       contains

       subroutine compute_Q(Q,T,k,m)
         implicit none
         type(VF),intent(inout) :: Q
         type(SF),intent(in) :: T
         type(VF),intent(in) :: k
         type(mesh),intent(in) :: m
         call grad(Q,T,m)
         call multiply(Q,k)
         call multiply(Q,-1.0_cp)
       end subroutine

       subroutine compute_divQ(divQ,Q,m)
         implicit none
         type(SF),intent(inout) :: divQ
         type(VF),intent(in) :: Q
         type(mesh),intent(in) :: m
         call div(divQ,Q,m)
       end subroutine

       subroutine volumetric_heating_intro(Q_CC,m)
         implicit none
         type(SF),intent(inout) :: Q_CC
         type(mesh),intent(in) :: m
         integer :: t,i,j,k
         real(cp) :: q_flux,a,L
         q_flux = 1.0_cp
         a = 1.0_cp
         L = 1.0_cp
         !$OMP PARALLEL DO
         do t=1,m%s; do k=1,Q_CC%BF(t)%GF%s(3); do j=1,Q_CC%BF(t)%GF%s(2); do i=1,Q_CC%BF(t)%GF%s(1)
         Q_CC%BF(t)%GF%f(i,j,k) = q_flux*exp(-(m%B(t)%g%c(2)%hc%f(j)+a)/L)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       subroutine volumetric_heating_equation(Q_CC,m,Pe)
         implicit none
         type(SF),intent(inout) :: Q_CC
         type(mesh),intent(in) :: m
         real(cp),intent(in) :: Pe
         integer :: t,i,j,k
         real(cp) :: a,L,m_,F
         a = 1.0_cp
         L = 1.0_cp
         m_ = a/L
         F = 2.0_cp*m_/(1.0_cp-exp(-2.0_cp*m_)) /Pe
         !$OMP PARALLEL DO
         do t=1,m%s; do k=1,Q_CC%BF(t)%GF%s(3); do j=1,Q_CC%BF(t)%GF%s(2); do i=1,Q_CC%BF(t)%GF%s(1)
         Q_CC%BF(t)%GF%f(i,j,k) = F*exp(-m_*(m%B(t)%g%c(2)%hc%f(j)+1.0_cp))
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
       end subroutine

       end module