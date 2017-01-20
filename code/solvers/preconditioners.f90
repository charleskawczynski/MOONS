      module preconditioners_mod
      use current_precision_mod
      use GF_diagonals_mod
      use data_location_mod
      use diagonals_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      use ops_aux_mod
      implicit none

      private
      public :: preconditioner_SF
      public :: preconditioner_VF

      public :: prec_Identity_SF
      public :: prec_Identity_VF
      public :: prec_Lap_SF
      public :: prec_Lap_VF
      public :: prec_mom_VF
      public :: prec_ind_VF

      abstract interface
        subroutine preconditioner_SF(Minv,m,sig,c)
          import mesh,SF,VF,cp
          implicit none
          type(SF),intent(inout) :: Minv
          type(mesh),intent(in) :: m
          type(VF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      abstract interface
        subroutine preconditioner_VF(Minv,m,sig,c)
          import mesh,VF,cp
          implicit none
          type(VF),intent(inout) :: Minv
          type(mesh),intent(in) :: m
          type(VF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      contains

      subroutine prec_Identity_SF(Minv,m,sig,c)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(SF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: sig
        real(cp),intent(in) :: c
        real(cp) :: suppress_warn
        logical :: suppress_warning
        suppress_warn = c
        suppress_warning = defined(sig%x%DL)
        suppress_warning = m%defined
        call assign(Minv,1.0_cp)
      end subroutine

      subroutine prec_Identity_VF(Minv,m,sig,c)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(VF),intent(inout) :: Minv
        type(VF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        real(cp) :: suppress_warn
        logical :: suppress_warning
        suppress_warn = c
        suppress_warning = defined(sig%x%DL)
        suppress_warning = m%defined
        call assign(Minv,1.0_cp)
      end subroutine

      subroutine prec_Lap_SF(Minv,m,sig,c)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: sig
        real(cp),intent(in) :: c
        type(SF) :: vol
        real(cp) :: suppress_warn
        logical :: suppress_warning
        suppress_warn = c
        suppress_warning = defined(sig%x%DL)
        call diag_Lap(Minv,m)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      subroutine prec_Lap_VF(Minv,m,sig,c)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv
        type(VF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_Lap_SF(Minv%x,m,sig,c)
        call prec_Lap_SF(Minv%y,m,sig,c)
        call prec_Lap_SF(Minv%z,m,sig,c)
      end subroutine

      subroutine prec_mom_SF(Minv,m,sig,c)
        ! Computes Laplacian diagonal preconditioner, Minv = diag( I + c∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: sig
        real(cp),intent(in) :: c
        type(SF) :: vol
        logical :: suppress_warning
        suppress_warning = defined(sig%x%DL)
        call assign(Minv,0.0_cp)
        call diag_Lap(Minv,m)
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      subroutine prec_mom_VF(Minv,m,sig,c)
        implicit none
        type(VF),intent(inout) :: Minv
        type(VF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_mom_SF(Minv%x,m,sig,c)
        call prec_mom_SF(Minv%y,m,sig,c)
        call prec_mom_SF(Minv%z,m,sig,c)
      end subroutine

      subroutine prec_ind_VF(Minv,m,sig,c) ! Verified 1/3/2016
        ! Computes curl-curl diagonal preconditioner: Minv = diag( V(I + c∇x(σ∇x)) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv
        type(VF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        type(VF) :: vol
        call diag_curl_curl(Minv,m,sig)
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      end module