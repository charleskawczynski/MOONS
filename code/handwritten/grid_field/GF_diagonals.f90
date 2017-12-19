      module GF_diagonals_mod
      use current_precision_mod
      use data_location_extend_mod
      use grid_mod
      use grid_field_mod
      use GF_assign_mod
      implicit none

      private
      public :: laplacian_diagonal
      public :: curl_curl_diagonal

      contains

      subroutine laplacian_diagonal(diag,g,DL)
        ! Computes Laplacian diagonal: diag = diag( ∇•(∇) )
        implicit none
        type(grid_field),intent(inout) :: diag
        type(grid),intent(in) :: g
        type(data_location),intent(in) :: DL
        integer :: i,j,k,pnx,pny,pnz
        integer,dimension(3) :: p
        call assign(diag,0.0_cp)
        p = N_eye(DL); pnx = p(1); pny = p(2); pnz = p(3)
        !$OMP PARALLEL DO
        do k=2,diag%s(3)-1; do j=2,diag%s(2)-1; do i=2,diag%s(1)-1
        diag%f(i,j,k) = g%c(1)%stagN2CC%D%f(  i  )*g%c(1)%stagCC2N%U%f( i-1 ) + &
                        g%c(1)%stagN2CC%U%f(i-pnx)*g%c(1)%stagCC2N%D%f(i-pnx) + &
                        g%c(2)%stagN2CC%D%f(  j  )*g%c(2)%stagCC2N%U%f( j-1 ) + &
                        g%c(2)%stagN2CC%U%f(j-pny)*g%c(2)%stagCC2N%D%f(j-pny) + &
                        g%c(3)%stagN2CC%D%f(  k  )*g%c(3)%stagCC2N%U%f( k-1 ) + &
                        g%c(3)%stagN2CC%U%f(k-pnz)*g%c(3)%stagCC2N%D%f(k-pnz)
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      subroutine curl_curl_diagonal(diag_x,diag_y,diag_z,g,sig_x,sig_y,sig_z) ! Verified 1/3/2016
        ! Computes curl-curl diagonal: diag = ∇x(σ∇x)
        implicit none
        type(grid_field),intent(inout) :: diag_x,diag_y,diag_z
        type(grid_field),intent(in) :: sig_x,sig_y,sig_z
        type(grid),intent(in) :: g
        integer :: i,j,k
        call assign(diag_x,0.0_cp)
        call assign(diag_y,0.0_cp)
        call assign(diag_z,0.0_cp)
        !$OMP PARALLEL DO
        do k=2,diag_x%s(3)-1; do j=2,diag_x%s(2)-1; do i=2,diag_x%s(1)-1
        diag_x%f(i,j,k) = -g%c(2)%stagN2CC%D%f(j)*sig_z%f(i,j, k )*g%c(2)%stagCC2N%U%f(j-1) - &
                           g%c(2)%stagN2CC%U%f(j)*sig_z%f(i,j+1,k)*g%c(2)%stagCC2N%D%f( j ) - &
                           g%c(3)%stagN2CC%D%f(k)*sig_y%f(i,j, k )*g%c(3)%stagCC2N%U%f(k-1) - &
                           g%c(3)%stagN2CC%U%f(k)*sig_y%f(i,j,k+1)*g%c(3)%stagCC2N%D%f( k )
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=2,diag_y%s(3)-1; do j=2,diag_y%s(2)-1; do i=2,diag_y%s(1)-1
        diag_y%f(i,j,k) = -g%c(1)%stagN2CC%D%f(i)*sig_z%f(i,j, k )*g%c(1)%stagCC2N%U%f(i-1) - &
                           g%c(1)%stagN2CC%U%f(i)*sig_z%f(i+1,j,k)*g%c(1)%stagCC2N%D%f( i ) - &
                           g%c(3)%stagN2CC%D%f(k)*sig_x%f(i,j, k )*g%c(3)%stagCC2N%U%f(k-1) - &
                           g%c(3)%stagN2CC%U%f(k)*sig_x%f(i,j,k+1)*g%c(3)%stagCC2N%D%f( k )
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
        !$OMP PARALLEL DO
        do k=2,diag_z%s(3)-1; do j=2,diag_z%s(2)-1; do i=2,diag_z%s(1)-1
        diag_z%f(i,j,k) = -g%c(1)%stagN2CC%D%f(i)*sig_y%f(i, j ,k)*g%c(1)%stagCC2N%U%f(i-1) - &
                           g%c(1)%stagN2CC%U%f(i)*sig_y%f(i+1,j,k)*g%c(1)%stagCC2N%D%f( i ) - &
                           g%c(2)%stagN2CC%D%f(j)*sig_x%f(i, j ,k)*g%c(2)%stagCC2N%U%f(j-1) - &
                           g%c(2)%stagN2CC%U%f(j)*sig_x%f(i,j+1,k)*g%c(2)%stagCC2N%D%f( j )
        enddo; enddo; enddo
        !$OMP END PARALLEL DO
      end subroutine

      end module