      module GF_Fourier_number_mod
        use grid_field_mod
        use grid_mod
        use current_precision_mod
        implicit none
        private
        public :: Fourier_number
        interface Fourier_number; module procedure Fourier_number_GF; end interface

        contains

        function Fourier_number_GF(alpha,g,dt) result(Fourier)
          ! Computes: Fo =  max(alpha*dt/dh^2)
          implicit none
          real(cp),intent(in) :: alpha,dt
          type(grid),intent(in) :: g
          real(cp),dimension(3) :: temp
          integer :: i,j,k
          real(cp) :: Fourier
          Fourier = 0.0_cp
          do k=1,g%c(3)%sc; do j=1,g%c(2)%sc; do i=1,g%c(1)%sc
            temp(1) = alpha / g%c(1)%dhn%f(i)**2.0_cp
            temp(2) = alpha / g%c(2)%dhn%f(j)**2.0_cp
            temp(3) = alpha / g%c(3)%dhn%f(k)**2.0_cp
            Fourier = maxval((/Fourier,dt*maxval(temp)/))
          enddo; enddo; enddo
        end function

      end module