      module diagonals_mod
      use current_precision_mod
      use GF_diagonals_mod
      use mesh_extend_mod
      use SF_extend_mod
      use VF_extend_mod
      use TF_mod
      implicit none

      private
      public :: diag_Lap
      public :: diag_curl_curl
      interface diag_Lap;         module procedure diag_Lap_SF;          end interface
      interface diag_Lap;         module procedure diag_Lap_VF;          end interface
      interface diag_curl_curl;   module procedure diag_curl_curl_VF;    end interface

      contains

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

      subroutine diag_Lap_VF(diag,m)
        implicit none
        type(VF),intent(inout) :: diag
        type(mesh),intent(in) :: m
        call diag_Lap(diag%x,m)
        call diag_Lap(diag%y,m)
        call diag_Lap(diag%z,m)
      end subroutine

      subroutine diag_curl_curl_VF(diag,m,sig) ! Verified 1/3/2016
        ! Computes curl-curl diagonal: diag = diag(∇x(σ∇x))
        implicit none
        type(VF),intent(inout) :: diag
        type(mesh),intent(in) :: m
        type(TF),intent(in) :: sig
        integer :: t
        call assign(diag,0.0_cp)
        do t=1,m%s
          call curl_curl_diagonal(diag%x%BF(t)%GF,&
                                  diag%y%BF(t)%GF,&
                                  diag%z%BF(t)%GF,&
                                  m%B(t)%g,&
                                  sig%x%x%BF(t)%GF,&
                                  sig%x%y%BF(t)%GF,&
                                  sig%x%z%BF(t)%GF)
        enddo
      end subroutine

      end module