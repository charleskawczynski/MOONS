      module matrix_free_params_extend_mod
      use matrix_free_params_mod
      use current_precision_mod
      implicit none

      private
      public :: prolongate
      interface prolongate; module procedure prolongate_MFP; end interface

      contains

      subroutine prolongate_MFP(m,coeff_multiplication_factor)
        implicit none
        type(matrix_free_params),intent(inout) :: m
        real(cp),intent(in) :: coeff_multiplication_factor
        m%coeff_natural = coeff_multiplication_factor*m%coeff_natural
        m%coeff_explicit = coeff_multiplication_factor*m%coeff_explicit
      end subroutine

      end module