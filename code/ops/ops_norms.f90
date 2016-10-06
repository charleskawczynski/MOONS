       module ops_norms_mod
       use current_precision_mod
       use mesh_mod
       use mesh_domain_mod
       use SF_mod
       use VF_mod
       use TF_mod
       implicit none

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
       interface Ln;    module procedure Ln_mesh_TF;           end interface

       interface Ln;    module procedure Ln_mesh_SF_D;         end interface
       interface Ln;    module procedure Ln_mesh_VF_D;         end interface
       interface Ln;    module procedure Ln_mesh_TF_D;         end interface

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
         ! if (.not.u%is_CC) stop 'Error: must use CC data in Ln_mesh_VF in ops_Ln_norms.f90'
         if (u%is_CC) then
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s; do k=2,m%B(t)%g%c(3)%sc-1; do j=2,m%B(t)%g%c(2)%sc-1; do i=2,m%B(t)%g%c(1)%sc-1
           eTemp = eTemp + (u%BF(t)%GF%f(i,j,k)**n)*m%vol(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
         elseif (u%is_Node) then
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,u%s; do k=3,m%B(t)%g%c(3)%sn-2; do j=3,m%B(t)%g%c(2)%sn-2; do i=3,m%B(t)%g%c(1)%sn-2
           eTemp = eTemp + (u%BF(t)%GF%f(i,j,k)**n)*m%B(t)%g%c(1)%dhc(i-1)*&
                                                 m%B(t)%g%c(2)%dhc(j-1)*&
                                                 m%B(t)%g%c(3)%dhc(k-1)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
         else; stop 'Error: must use CC/N data in Ln_mesh_VF in ops_Ln_norms.f90'
         endif
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
         do t=1,m%s; do k=2,m%B(t)%g%c(3)%sc-1; do j=2,m%B(t)%g%c(2)%sc-1; do i=2,m%B(t)%g%c(1)%sc-1
           eTemp = eTemp + (u%x%BF(t)%GF%f(i,j,k)**n+&
                            u%y%BF(t)%GF%f(i,j,k)**n+&
                            u%z%BF(t)%GF%f(i,j,k)**n)*m%vol(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_mesh_TF(e,u,n,m)
         ! Computes
         ! 
         !   L(n) = ∫∫∫ | u(i,j,k)ⁿ | dx dy dz
         ! 
         implicit none
         real(cp),intent(inout) :: e
         type(TF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: eTemp
         integer :: i,j,k,t
         if (.not.u%is_CC) stop 'Error: must use CC data in Ln_mesh_TF in ops_Ln_norms.f90'
         eTemp = 0.0_cp ! temp is necessary for reduction
         !$OMP PARALLEL DO REDUCTION(+:eTemp)
         do t=1,m%s; do k=2,m%B(t)%g%c(3)%sc-1; do j=2,m%B(t)%g%c(2)%sc-1; do i=2,m%B(t)%g%c(1)%sc-1
           eTemp = eTemp + (u%x%x%BF(t)%GF%f(i,j,k)**n+&
                            u%x%y%BF(t)%GF%f(i,j,k)**n+&
                            u%x%z%BF(t)%GF%f(i,j,k)**n+&
                            u%y%x%BF(t)%GF%f(i,j,k)**n+&
                            u%y%y%BF(t)%GF%f(i,j,k)**n+&
                            u%y%z%BF(t)%GF%f(i,j,k)**n+&
                            u%z%x%BF(t)%GF%f(i,j,k)**n+&
                            u%z%y%BF(t)%GF%f(i,j,k)**n+&
                            u%z%z%BF(t)%GF%f(i,j,k)**n)*m%vol(t)%f(i,j,k)
         enddo; enddo; enddo; enddo
         !$OMP END PARALLEL DO
         e = eTemp
       end subroutine

       subroutine Ln_mesh_SF_D(e,u,n,m,MD)
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         if (compare(m,MD%m_R1)) then;    call Ln(e,u,n,MD%m_R2)
         elseif(compare(m,MD%m_R2)) then; call Ln(e,u,n,MD%m_R1)
         else; stop 'Error: missed case in Ln_mesh_SF_D in ops_norms.f90'
         endif
       end subroutine
       subroutine Ln_mesh_VF_D(e,u,n,m,MD)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         if (compare(m,MD%m_R1)) then;    call Ln(e,u,n,MD%m_R2)
         elseif(compare(m,MD%m_R2)) then; call Ln(e,u,n,MD%m_R1)
         else; stop 'Error: missed case in Ln_mesh_VF_D in ops_norms.f90'
         endif
       end subroutine
       subroutine Ln_mesh_TF_D(e,u,n,m,MD)
         implicit none
         real(cp),intent(inout) :: e
         type(TF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
         if (compare(m,MD%m_R1)) then;    call Ln(e,u,n,MD%m_R2)
         elseif(compare(m,MD%m_R2)) then; call Ln(e,u,n,MD%m_R1)
         else; stop 'Error: missed case in Ln_mesh_TF_D in ops_norms.f90'
         endif
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
         do t=1,u%s; do k=2,u%BF(t)%GF%s(3)-1; do j=2,u%BF(t)%GF%s(2)-1; do i=2,u%BF(t)%GF%s(1)-1
           eTemp = eTemp + (u%BF(t)%GF%f(i,j,k)**n)*vol%BF(t)%GF%f(i,j,k)
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
         do t=1,u%x%s; do k=2,u%x%BF(t)%GF%s(3)-1; do j=2,u%x%BF(t)%GF%s(2)-1; do i=2,u%x%BF(t)%GF%s(1)-1
           eTemp = eTemp + (u%x%BF(t)%GF%f(i,j,k)**n+&
                            u%y%BF(t)%GF%f(i,j,k)**n+&
                            u%z%BF(t)%GF%f(i,j,k)**n)*vol%BF(t)%GF%f(i,j,k)
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
           do t=1,u%x%s; do k=2,u%x%BF(t)%GF%s(3)-1; do j=2,u%x%BF(t)%GF%s(2)-1; do i=2,u%x%BF(t)%GF%s(1)-1
             eTemp = eTemp + (u%x%BF(t)%GF%f(i,j,k)**n)*vol%x%BF(t)%GF%f(i,j,k)
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%y%s; do k=2,u%y%BF(t)%GF%s(3)-1; do j=2,u%y%BF(t)%GF%s(2)-1; do i=2,u%y%BF(t)%GF%s(1)-1
             eTemp = eTemp + (u%y%BF(t)%GF%f(i,j,k)**n)*vol%y%BF(t)%GF%f(i,j,k)
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%z%s; do k=2,u%z%BF(t)%GF%s(3)-1; do j=2,u%z%BF(t)%GF%s(2)-1; do i=2,u%z%BF(t)%GF%s(1)-1
             eTemp = eTemp + (u%z%BF(t)%GF%f(i,j,k)**n)*vol%z%BF(t)%GF%f(i,j,k)
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
         do t=1,u%s; do k=2,u%BF(t)%GF%s(3)-1; do j=2,u%BF(t)%GF%s(2)-1; do i=2,u%BF(t)%GF%s(1)-1
           eTemp = eTemp + (u%BF(t)%GF%f(i,j,k)**n)
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
           do t=1,u%x%s; do k=2,u%x%BF(t)%GF%s(3)-1; do j=2,u%x%BF(t)%GF%s(2)-1; do i=2,u%x%BF(t)%GF%s(1)-1
             eTemp = eTemp + u%x%BF(t)%GF%f(i,j,k)**n+&
                             u%y%BF(t)%GF%f(i,j,k)**n+&
                             u%z%BF(t)%GF%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp
         else
           eTemp = 0.0_cp ! temp is necessary for reduction
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%x%s; do k=2,u%x%BF(t)%GF%s(3)-1; do j=2,u%x%BF(t)%GF%s(2)-1; do i=2,u%x%BF(t)%GF%s(1)-1
             eTemp = eTemp + u%x%BF(t)%GF%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%y%s; do k=2,u%y%BF(t)%GF%s(3)-1; do j=2,u%y%BF(t)%GF%s(2)-1; do i=2,u%y%BF(t)%GF%s(1)-1
             eTemp = eTemp + u%y%BF(t)%GF%f(i,j,k)**n
           enddo; enddo; enddo; enddo
           !$OMP END PARALLEL DO
           e = e+eTemp; eTemp = 0.0_cp
           !$OMP PARALLEL DO REDUCTION(+:eTemp)
           do t=1,u%z%s; do k=2,u%z%BF(t)%GF%s(3)-1; do j=2,u%z%BF(t)%GF%s(2)-1; do i=2,u%z%BF(t)%GF%s(1)-1
             eTemp = eTemp + u%z%BF(t)%GF%f(i,j,k)**n
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