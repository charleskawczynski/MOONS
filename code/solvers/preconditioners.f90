      module preconditioners_mod
      use current_precision_mod
      use GF_diagonals_mod
      use mesh_mod
      use SF_mod
      use VF_mod
      use ops_aux_mod
      implicit none

      private
      public :: prec_Identity_SF
      public :: prec_Identity_VF
      public :: prec_Lap_SF,diag_Lap_SF
      public :: prec_Lap_VF,diag_Lap_VF
      public :: prec_mom_VF
      public :: prec_ind_VF

      contains

      subroutine prec_Identity_SF(Minv)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(SF),intent(inout) :: Minv
        call assign(Minv,1.0_cp)
      end subroutine

      subroutine prec_Identity_VF(Minv)
        ! Computes Identity preconditioner (no preconditioning): Minv = I
        implicit none
        type(VF),intent(inout) :: Minv
        call assign(Minv,1.0_cp)
      end subroutine

      subroutine diag_Lap_SF(diag,m)
        ! Computes Laplacian diagonal: diag = diag( ∇•(∇) )
        implicit none
        type(SF),intent(inout) :: diag
        type(mesh),intent(in) :: m
        integer :: t
        do t=1,m%s
          call laplacian_diagonal(diag%BF(t)%GF,m%B(t)%g,diag%DL)
        enddo
      end subroutine
      subroutine prec_Lap_SF(Minv,m)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        type(SF) :: vol
        call diag_Lap_SF(Minv,m)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call zeroGhostPoints(Minv)
      end subroutine

      subroutine diag_Lap_VF(diag,m)
        ! Computes Laplacian diagonal: diag = diag( ∇•(∇) )
        implicit none
        type(VF),intent(inout) :: diag
        type(mesh),intent(in) :: m
        call diag_Lap_SF(diag%x,m)
        call diag_Lap_SF(diag%y,m)
        call diag_Lap_SF(diag%z,m)
      end subroutine
      subroutine prec_Lap_VF(Minv,m)
        ! Computes Laplacian diagonal preconditioner: Minv = diag( V ∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        call prec_Lap_SF(Minv%x,m)
        call prec_Lap_SF(Minv%y,m)
        call prec_Lap_SF(Minv%z,m)
      end subroutine

      subroutine prec_mom_SF(Minv,m,c)
        ! Computes Laplacian diagonal preconditioner, Minv = diag( I + c∇•(∇) )⁻¹, V = cell volume
        implicit none
        type(SF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        type(SF) :: vol
        call assign(Minv,0.0_cp)
        call diag_Lap_SF(Minv,m)
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call zeroGhostPoints(Minv)
      end subroutine

      subroutine prec_mom_VF(Minv,m,c)
        implicit none
        type(VF),intent(inout) :: Minv
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        call prec_mom_SF(Minv%x,m,c)
        call prec_mom_SF(Minv%y,m,c)
        call prec_mom_SF(Minv%z,m,c)
      end subroutine

      subroutine prec_ind_VF(Minv,m,sig,c) ! Verified 1/3/2016
        ! Computes curl-curl diagonal preconditioner: Minv = diag( V(I + c∇x(σ∇x)) )⁻¹, V = cell volume
        implicit none
        type(VF),intent(inout) :: Minv
        type(VF),intent(in) :: sig
        type(mesh),intent(in) :: m
        real(cp),intent(in) :: c
        type(VF) :: vol
        integer :: t
        call assign(Minv,0.0_cp)
        do t=1,m%s
          call curl_curl_diagonal(Minv%x%BF(t)%GF,&
                                  Minv%y%BF(t)%GF,&
                                  Minv%z%BF(t)%GF,&
                                  m%B(t)%g,&
                                  sig%x%BF(t)%GF,&
                                  sig%y%BF(t)%GF,&
                                  sig%z%BF(t)%GF)
        enddo
        call multiply(Minv,c)
        call add(Minv,1.0_cp)
        call init(vol,Minv)
        call volume(vol,m)
        call multiply(Minv,vol)
        call delete(vol)
        call invert(Minv)
        call zeroGhostPoints(Minv)
      end subroutine

      end module