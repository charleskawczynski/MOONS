      module preconditioners_mod
      use current_precision_mod
      use GF_diagonals_mod
      use data_location_mod
      use diagonals_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      use TF_mod
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
        subroutine preconditioner_SF(Minv,m,sig,c,temp_Minv)
          import mesh,SF,TF,cp
          implicit none
          type(SF),intent(inout) :: Minv,temp_Minv
          type(mesh),intent(in) :: m
          type(TF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      abstract interface
        subroutine preconditioner_VF(Minv,m,sig,c,temp_Minv)
          import mesh,VF,TF,cp
          implicit none
          type(VF),intent(inout) :: Minv,temp_Minv
          type(mesh),intent(in) :: m
          type(TF),intent(in) :: sig
          real(cp),intent(in) :: c
        end subroutine
      end interface

      contains

      subroutine prec_Identity_SF(Minv,m,sig,c,temp_Minv)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(SF),intent(inout) :: Minv,temp_Minv
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: sig
        real(cp),intent(in) :: c
        real(cp) :: suppress_warn
        logical :: suppress_warning
        suppress_warn = c
        suppress_warning = defined(sig%x%x%DL)
        suppress_warning = defined(temp_Minv%DL)
        suppress_warning = m%defined
        call assign(Minv,1.0_cp)
      end subroutine

      subroutine prec_Identity_VF(Minv,m,sig,c,temp_Minv)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(VF),intent(inout) :: Minv,temp_Minv
        type(TF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_Identity_SF(Minv%x,m,sig,c,temp_Minv%x)
        call prec_Identity_SF(Minv%y,m,sig,c,temp_Minv%y)
        call prec_Identity_SF(Minv%z,m,sig,c,temp_Minv%z)
      end subroutine

      subroutine prec_Lap_SF(Minv,m,sig,c,temp_Minv)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv,temp_Minv
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: sig
        real(cp),intent(in) :: c
        real(cp) :: suppress_warn
        logical :: suppress_warning
        suppress_warn = c
        suppress_warning = defined(sig%x%x%DL)
        call diag_Lap(Minv,m)
        call volume(temp_Minv,m)
        call multiply(Minv,temp_Minv)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      subroutine prec_Lap_VF(Minv,m,sig,c,temp_Minv)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv,temp_Minv
        type(TF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_Lap_SF(Minv%x,m,sig,c,temp_Minv%x)
        call prec_Lap_SF(Minv%y,m,sig,c,temp_Minv%y)
        call prec_Lap_SF(Minv%z,m,sig,c,temp_Minv%z)
      end subroutine

      subroutine prec_mom_SF(Minv,m,sig,c,temp_Minv)
        ! Computes Laplacian diagonal preconditioner, Minv = diag( I + c∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv,temp_Minv
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: sig
        real(cp),intent(in) :: c
        logical :: suppress_warning
        suppress_warning = defined(sig%x%x%DL)
        call assign(Minv,0.0_cp)
        call diag_Lap(Minv,m)
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call volume(temp_Minv,m)
        call multiply(Minv,temp_Minv)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      subroutine prec_mom_VF(Minv,m,sig,c,temp_Minv)
        implicit none
        type(VF),intent(inout) :: Minv,temp_Minv
        type(TF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_mom_SF(Minv%x,m,sig,c,temp_Minv%x)
        call prec_mom_SF(Minv%y,m,sig,c,temp_Minv%y)
        call prec_mom_SF(Minv%z,m,sig,c,temp_Minv%z)
      end subroutine

      subroutine prec_ind_VF(Minv,m,sig,c,temp_Minv) ! Verified 1/3/2016
        ! Computes curl-curl diagonal preconditioner: Minv = diag( V(I + c∇x(σ∇x)) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv,temp_Minv
        type(TF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call diag_curl_curl(Minv,m,sig)
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call volume(temp_Minv,m)
        call multiply(Minv,temp_Minv)
        call invert(Minv)
        call assign_ghost_XPeriodic(Minv,0.0_cp)
      end subroutine

      end module