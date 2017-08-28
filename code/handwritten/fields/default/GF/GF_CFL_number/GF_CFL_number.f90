      module GF_CFL_number_mod
        use GF_base_mod
        use GF_aux_mod
        use grid_mod
        use current_precision_mod
        implicit none
        private
        public :: CFL_number
        interface CFL_number;          module procedure CFL_number_GF;          end interface
        public :: dt_given_CFL_number
        interface dt_given_CFL_number; module procedure dt_given_CFL_number_GF; end interface

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

        function dt_given_CFL_number_GF(U_CC,V_CC,W_CC,g,CFL) result(dt)
          ! Computes: dt =  CFL/max(sum(u_i/dx_i)), i=1,3
          implicit none
          type(grid_field),intent(in) :: U_CC,V_CC,W_CC
          type(grid),intent(in) :: g
          real(cp),intent(in) :: CFL
          real(cp),dimension(3) :: temp
          real(cp) :: temp_sum,u_max
          integer :: i,j,k
          real(cp) :: dt
          ! real(cp) :: dh_min
          temp_sum = 0.0_cp
          u_max = maxval((/amax(U_CC),amax(V_CC),amax(W_CC)/))
          do k=1,U_CC%s(3); do j=1,U_CC%s(2); do i=1,U_CC%s(1)
            ! temp(1) = g%c(1)%dhn%f(i)/abs(U_CC%f(i,j,k))
            ! temp(2) = g%c(2)%dhn%f(j)/abs(V_CC%f(i,j,k))
            ! temp(3) = g%c(3)%dhn%f(k)/abs(W_CC%f(i,j,k))
            temp(1) = g%c(1)%dhn%f(i)/u_max
            temp(2) = g%c(2)%dhn%f(j)/u_max
            temp(3) = g%c(3)%dhn%f(k)/u_max
            temp_sum = maxval( (/temp_sum,sum(temp)/) )
          enddo; enddo; enddo
          ! dh_min = minval((/g%c(1)%dhMin,g%c(2)%dhMin,g%c(3)%dhMin/))
          ! dt = minval((/dt_initial,CFL/temp_sum/))
          dt = CFL/temp_sum
        end function

      end module