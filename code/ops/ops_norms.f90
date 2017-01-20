       module ops_norms_mod
       use current_precision_mod
       use mesh_mod
       use mesh_domain_mod
       use data_location_mod
       use GF_norms_mod
       use GF_norms_weights_mod
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
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         do t=1,m%s
          call Ln(temp,u%BF(t)%GF,n,m%B(t)%vol(vol_ID(u%DL)),'Ln_mesh_SF (2)')
          e = e + temp
         enddo
       end subroutine

       subroutine Ln_mesh_VF(e,u,n,m)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         do t=1,m%s;
           call Ln(temp,u%x%BF(t)%GF,&
                        u%y%BF(t)%GF,&
                        u%z%BF(t)%GF,&
                        n,&
                        m%B(t)%vol(vol_ID(u%x%DL)))
           e = e + temp
         enddo
       end subroutine

       subroutine Ln_mesh_TF(e,u,n,m)
         implicit none
         real(cp),intent(inout) :: e
         type(TF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
#ifdef _DEBUG_OPS_NORMS_
         if (is_collocated(u)) then
           stop 'Error: must use collocated data in Ln_mesh_TF in ops_norms.f90.f90'
         endif
#endif
         do t=1,m%s
           call Ln(temp,u%x%x%BF(t)%GF,&
                        u%x%y%BF(t)%GF,&
                        u%x%z%BF(t)%GF,&
                        u%y%x%BF(t)%GF,&
                        u%y%y%BF(t)%GF,&
                        u%y%z%BF(t)%GF,&
                        u%z%x%BF(t)%GF,&
                        u%z%y%BF(t)%GF,&
                        u%z%z%BF(t)%GF,&
                        n,m%B(t)%vol(vol_ID(u%x%x%DL)))
           e = e + temp
         enddo
       end subroutine

       subroutine Ln_mesh_SF_D(e,u,n,m,MD)
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         type(mesh),intent(in) :: m
         type(mesh_domain),intent(in) :: MD
            if (compare(m,MD%m_R1)) then; call Ln(e,u,n,MD%m_R2)
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
            if (compare(m,MD%m_R1)) then; call Ln(e,u,n,MD%m_R2)
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
            if (compare(m,MD%m_R1)) then; call Ln(e,u,n,MD%m_R2)
         elseif(compare(m,MD%m_R2)) then; call Ln(e,u,n,MD%m_R1)
         else; stop 'Error: missed case in Ln_mesh_TF_D in ops_norms.f90'
         endif
       end subroutine

       subroutine Ln_vol_SF(e,u,n,vol)
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u,vol
         real(cp),intent(in) :: n
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         do t=1,u%s
           call Ln(temp,u%BF(t)%GF,n,vol%BF(t)%GF,'Ln_vol_SF')
           e = e + temp
         enddo
       end subroutine

       subroutine Ln_vol_VF_collocated(e,u,n,vol)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         type(SF),intent(in) :: vol
         real(cp),intent(in) :: n
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         do t=1,vol%s
           call Ln(temp,u%x%BF(t)%GF,u%y%BF(t)%GF,u%z%BF(t)%GF,n,vol%BF(t)%GF)
           e = e + temp
         enddo
       end subroutine

       subroutine Ln_vol_VF(e,u,n,vol)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u,vol
         real(cp),intent(in) :: n
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         if (is_collocated(u)) then; call Ln(e,u,n,vol%x)
         else
           do t=1,u%x%s
             call Ln(temp,u%x%BF(t)%GF,n,vol%x%BF(t)%GF,'Ln_vol_VF'); e = e + temp
             call Ln(temp,u%y%BF(t)%GF,n,vol%y%BF(t)%GF,'Ln_vol_VF'); e = e + temp
             call Ln(temp,u%z%BF(t)%GF,n,vol%z%BF(t)%GF,'Ln_vol_VF'); e = e + temp
           enddo
         endif
       end subroutine

       subroutine Ln_no_vol_SF(e,u,n)
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         do t=1,u%s
           call Ln(temp,u%BF(t)%GF,n)
           e = e + temp
         enddo
       end subroutine

       subroutine Ln_no_vol_VF(e,u,n)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         real(cp),intent(in) :: n
         real(cp) :: temp
         integer :: t
         e = 0.0_cp
         if (is_collocated(u)) then
           do t=1,u%x%s
             call Ln(temp,u%x%BF(t)%GF,u%y%BF(t)%GF,u%z%BF(t)%GF,n); e = e + temp
           enddo
         else
           do t=1,u%x%s
             call Ln(temp,u%x%BF(t)%GF,n); e = e + temp
             call Ln(temp,u%y%BF(t)%GF,n); e = e + temp
             call Ln(temp,u%z%BF(t)%GF,n); e = e + temp
           enddo
         endif
       end subroutine

       subroutine Linf_SF(e,u)
         implicit none
         real(cp),intent(inout) :: e
         type(SF),intent(in) :: u
         e = amax(u)
       end subroutine

       subroutine Linf_VF(e,u)
         implicit none
         real(cp),intent(inout) :: e
         type(VF),intent(in) :: u
         e = amax(u)
       end subroutine

       end module