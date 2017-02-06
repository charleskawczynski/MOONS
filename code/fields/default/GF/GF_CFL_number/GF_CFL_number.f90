      module GF_CFL_number_mod
        use GF_base_mod
        use grid_mod
        use current_precision_mod
        implicit none
        private
        public :: CFL_number
        interface CFL_number; module procedure CFL_number_GF; end interface

        contains

        function CFL_number_GF(U_CC,V_CC,W_CC,g,dt) result(CFL)
          ! Computes: CFL =  max(sum(u_i/dx_i)), i=1,3
          implicit none
          type(grid_field),intent(in) :: U_CC,V_CC,W_CC
          type(grid),intent(in) :: g
          real(cp),intent(in) :: dt
          real(cp),dimension(3) :: temp
          integer :: i,j,k
          real(cp) :: CFL
          CFL = 0.0_cp
          do k=1,U_CC%s(3); do j=1,U_CC%s(2); do i=1,U_CC%s(1)
            temp(1) = abs(U_CC%f(i,j,k)) / g%c(1)%dhn%f(i)
            temp(2) = abs(V_CC%f(i,j,k)) / g%c(2)%dhn%f(j)
            temp(3) = abs(W_CC%f(i,j,k)) / g%c(3)%dhn%f(k)
            CFL = maxval((/CFL,dt*sum(temp)/))
          enddo; enddo; enddo
        end function

      end module