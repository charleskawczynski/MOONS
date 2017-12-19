      module GF_Robin_BC_coeff_mod
        use grid_field_mod
        use grid_mod
        use face_edge_corner_indexing_mod
        use current_precision_mod
        implicit none
        private
        public :: Robin_BC_coeff
        interface Robin_BC_coeff; module procedure Robin_BC_coeff_GF; end interface

        contains

        function Robin_BC_coeff_GF(c_w,g) result(coeff)
          ! Computes:
          !
          !                           | 2*c_w - dh*nhat |
          ! Robin_BC_coeff =  maxval( ------------------- )
          !                           | 2*c_w + dh*nhat |
          !
          ! Which has a singularity at 2*c_w = - dh*nhat
          ! This is entirely possible if nhat = -1 and dh = 2*c_w
          ! Robin BCs will fail here and a different mesh size must be used!
          implicit none
          real(cp),dimension(6),intent(in) :: c_w
          type(grid),intent(in) :: g
          real(cp),dimension(6) :: coeff
          real(cp) :: nhat,dh
          integer :: i,dir
          dh = 1.0_cp ! suppress warning
          do i=1,6
            nhat = nhat_given_face(i)
            dir = dir_given_face(i)
            if (min_face(i)) dh = g%c(dir)%dhn%f(1)
            if (max_face(i)) dh = g%c(dir)%dhn_e
            coeff(i) = (2.0_cp*c_w(i)/dh*nhat-1.0_cp)/(2.0_cp*c_w(i)/dh*nhat+1.0_cp)
          enddo
        end function

      end module