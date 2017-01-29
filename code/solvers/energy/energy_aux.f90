       module energy_aux_mod
       use current_precision_mod
       use SF_mod
       use VF_mod

       use ops_embedExtract_mod
       use mesh_domain_mod
       use mesh_mod
       use ops_aux_mod
       use ops_interp_mod
       use ops_discrete_mod

       implicit none

       private
       public :: compute_AddBuoyancy
       public :: compute_AddGravity
       public :: computeBuoyancy
       public :: computeGravity
       public :: compute_Q
       public :: volumetric_heating_intro
       public :: volumetric_heating_equation
       public :: compute_divQ
       public :: embed_velocity_F

       contains

       subroutine computeBuoyancy(buoyancy,T,gravity,Gr,Re,m,MD,temp_F,temp_CC)
         ! Computes
         !
         !            Gr
         !           ---  T g
         !           Re^2
         implicit none
         type(VF),intent(inout) :: buoyancy,temp_F,temp_CC
         type(SF),intent(in) :: T
         type(VF),intent(in) :: gravity
         real(cp),intent(in) :: Gr,Re
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call assign(temp_CC,T)
         call multiply(temp_CC,Gr/(Re**2.0_cp))
         call multiply(temp_CC,gravity)
         call cellCenter2Face(temp_F,temp_CC,m)
         call extractFace(buoyancy,temp_F,MD)
       end subroutine

       subroutine compute_AddBuoyancy(buoyancy,T,gravity,Gr,Re,m,MD,temp_F,temp_CC,temp_buoyancy)
         implicit none
         type(VF),intent(inout) :: buoyancy,temp_F,temp_CC,temp_buoyancy
         type(SF),intent(in) :: T
         type(VF),intent(in) :: gravity
         real(cp),intent(in) :: Gr,Re
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call computeBuoyancy(temp_buoyancy,T,gravity,Gr,Re,m,MD,temp_F,temp_CC)
         call add(buoyancy,temp_buoyancy)
       end subroutine

       subroutine computeGravity(gravity,g,Fr,m,MD,temp_F,temp_CC)
         ! Computes
         !
         !            1
         !           --- g
         !           Fr^2
         implicit none
         type(VF),intent(inout) :: gravity,temp_F,temp_CC
         type(VF),intent(in) :: g
         real(cp),intent(in) :: Fr
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call assign(temp_CC,g)
         call multiply(temp_CC,1.0_cp/(Fr**2.0_cp))
         call cellCenter2Face(temp_F,temp_CC,m)
         call extractFace(gravity,temp_F,MD)
       end subroutine

       subroutine compute_AddGravity(gravity,g,Fr,m,MD,temp_F,temp_CC,temp_gravity)
         implicit none
         type(VF),intent(inout) :: gravity,temp_F,temp_CC,temp_gravity
         type(VF),intent(in) :: g
         real(cp),intent(in) :: Fr
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         call computeGravity(temp_gravity,g,Fr,m,MD,temp_F,temp_CC)
         call add(gravity,temp_gravity)
       end subroutine

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
         ! call subtract_physical_mean(Q_CC)
       end subroutine

       subroutine embed_velocity_F(U_Ft,U_Fi,MD)
         implicit none
         type(VF),intent(inout) :: U_Ft
         type(VF),intent(in) :: U_Fi ! Raw momentum velocity
         type(mesh_domain),intent(in) :: MD
         call embedFace(U_Ft,U_Fi,MD)
       end subroutine

       end module