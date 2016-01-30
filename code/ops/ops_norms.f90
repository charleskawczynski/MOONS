       module ops_norms_mod
       use mesh_mod
       use SF_mod
       use VF_mod
       implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       private
       public :: Ln
       public :: Linf

       ! Interfaces:
       ! Ln(e,u,n,mesh) = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz   ! Only for CC data
       ! Ln(e,u,n,vol)  = L(n) = ∫∫∫ u(i,j,k)ⁿ dx dy dz
       ! Ln(e,u,n)      = L(n) = ΣΣΣ u(i,j,k)ⁿ
       ! Linf(e,u)      = L(∞) = max(abs(u))

       interface Ln;    module procedure Ln_vol_SF;            end interface
       interface Ln;    module procedure Ln_vol_VF;            end interface
       interface Ln;    module procedure Ln_vol_VF_collocated; end interface
       interface Ln;    module procedure Ln_mesh_SF;           end interface
       interface Ln;    module procedure Ln_mesh_VF;           end interface

       interface Ln;    module procedure Ln_no_vol_SF;         end interface
       interface Ln;    module procedure Ln_no_vol_VF;         end interface

       interface Linf;  module procedure Linf_SF;              end interface
       interface Linf;  module procedure Linf_VF;              end interface

       contains

       subroutine Ln_mesh_SF(e,u,n,m)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: eTemp
         integer :: i,j,k,t
         if (.not.u%is_CC) stop 'Error: must use CC data in Ln_mesh_VF in ops_Ln_norms.f90'
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s; do k=2,m%g(t)%c(3)%sc-1; do j=2,m%g(t)%c(2)%sc-1; do i=2,m%g(t)%c(1)%sc-1
           eTemp = eTemp + (u%RF(t)%f(i,j,k)**n)*m%vol(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_mesh_VF(e,u,n,m)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: eTemp
         integer :: i,j,k,t
         if (.not.u%is_CC) stop 'Error: must use CC data in Ln_mesh_VF in ops_Ln_norms.f90'
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,m%s; do k=2,m%g(t)%c(3)%sc-1; do j=2,m%g(t)%c(2)%sc-1; do i=2,m%g(t)%c(1)%sc-1
           eTemp = eTemp + (u%x%RF(t)%f(i,j,k)**n+&
                            u%y%RF(t)%f(i,j,k)**n+&
                            u%z%RF(t)%f(i,j,k)**n)*m%vol(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_vol_SF(e,u,n,vol)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u,vol
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
           eTemp = eTemp + (u%RF(t)%f(i,j,k)**n)*vol%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_vol_VF_collocated(e,u,n,vol)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         type(SF),intent(in) :: vol
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%x%s; do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
           eTemp = eTemp + (u%x%RF(t)%f(i,j,k)**n+&
                            u%y%RF(t)%f(i,j,k)**n+&
                            u%z%RF(t)%f(i,j,k)**n)*vol%RF(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_vol_VF(e,u,n,vol)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u,vol
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         if (u%is_CC.or.u%is_Node) then; call Ln(e,u,n,vol%x)
         else
           eTemp = 0.0_cp ! temp is necessary for reduction
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%x%s; do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + (u%x%RF(t)%f(i,j,k)**n)*vol%x%RF(t)%f(i,j,k)
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%y%s; do k=2,u%y%RF(t)%s(3)-1; do j=2,u%y%RF(t)%s(2)-1; do i=2,u%y%RF(t)%s(1)-1
             eTemp = eTemp + (u%y%RF(t)%f(i,j,k)**n)*vol%y%RF(t)%f(i,j,k)
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%z%s; do k=2,u%z%RF(t)%s(3)-1; do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
             eTemp = eTemp + (u%z%RF(t)%f(i,j,k)**n)*vol%z%RF(t)%f(i,j,k)
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp
         endif
       end subroutine

       subroutine Ln_no_vol_SF(e,u,n)
         ! Computes
         ! 
         !   L(n) = ΣΣΣ | u(i,j,k)ⁿ |
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s; do k=2,u%RF(t)%s(3)-1; do j=2,u%RF(t)%s(2)-1; do i=2,u%RF(t)%s(1)-1
           eTemp = eTemp + (u%RF(t)%f(i,j,k)**n)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_no_vol_VF(e,u,n)
         ! Computes
         ! 
         !   L(n) = ΣΣΣ | u(i,j,k)ⁿ |
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: eTemp
         integer :: i,j,k,t
         if (u%is_CC.or.u%is_Node) then
           eTemp = 0.0_cp ! temp is necessary for reduction
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%x%s; do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + u%x%RF(t)%f(i,j,k)**n+&
                             u%y%RF(t)%f(i,j,k)**n+&
                             u%z%RF(t)%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp
         else
           eTemp = 0.0_cp ! temp is necessary for reduction
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%x%s; do k=2,u%x%RF(t)%s(3)-1; do j=2,u%x%RF(t)%s(2)-1; do i=2,u%x%RF(t)%s(1)-1
             eTemp = eTemp + u%x%RF(t)%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%y%s; do k=2,u%y%RF(t)%s(3)-1; do j=2,u%y%RF(t)%s(2)-1; do i=2,u%y%RF(t)%s(1)-1
             eTemp = eTemp + u%y%RF(t)%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%z%s; do k=2,u%z%RF(t)%s(3)-1; do j=2,u%z%RF(t)%s(2)-1; do i=2,u%z%RF(t)%s(1)-1
             eTemp = eTemp + u%z%RF(t)%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp
         endif
       end subroutine

       subroutine Linf_SF(e,u)
         ! Computes
         ! 
         !   L(∞) = max(abs(u))
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         e = maxabs(u)
       end subroutine

       subroutine Linf_VF(e,u)
         ! Computes
         ! 
         !   L(∞) = max(abs(u))
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         e = maxabs(u)
       end subroutine

       end module