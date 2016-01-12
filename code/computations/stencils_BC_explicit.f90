      module stencils_BC_explicit_mod
      ! This module contains all of the stencils used to compute all derivatives
      ! 
      ! INPUT:
      !     f            = f(:)
      !     dfdh         = dfdhf(:)
      !     T            = tridiagonal matrix, containing L,D,U
      !     s            = size(f)
      !     sdfdh        = size(dfdh)
      ! 
      ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
      ! 
      ! CharlieKawczynski@gmail.com

      use triDiag_mod
      implicit none

      private
      public :: stag_CC2N_exp
      public :: stag_N2CC_exp
      public :: collocated
      public :: collocated_1_sided_CC
      public :: collocated_1_sided_N

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      contains

      function staggered_CC2N(f,T,s,sdfdh) result(dfdh)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh) :: dfdh
        integer :: i
        do i=1,s-1
          dfdh(i+1) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end function

      function staggered_N2CC(f,T,s,sdfdh) result(dfdh)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh) :: dfdh
        integer :: i
        do i=2,s-1
          dfdh(i) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end function

      function collocated(f,T,s) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s
        real(cp),dimension(s) :: dfdh
        integer :: i
        do i=2,s-1
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end function

      function collocated_1_sided_CC(f,T,s,fb1,fb2) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        real(cp),intent(in) :: fb1,fb2
        integer,intent(in) :: s
        real(cp),dimension(s) :: dfdh
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(2) = fb1 *T%L(1) + &
                  f(2)*T%D(1) + &
                  f(3)*T%U(1)
        dfdh(s-1) = f(s-2)*T%L(s-2) + &
                    f(s-1)*T%D(s-2) + &
                      fb2 *T%U(s-2)
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end function

      function collocated_1_sided_N(f,T,s,fb1,fb2) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        real(cp),intent(in) :: fb1,fb2
        integer,intent(in) :: s
        real(cp),dimension(s) :: dfdh
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(2) = f(2)*T%L(1) + &
                  f(3)*T%D(1) + &
                  f(4)*T%U(1)
        dfdh(s-1) = f(s-3)*T%L(s-2) + &
                    f(s-2)*T%D(s-2) + &
                    f(s-1)*T%U(s-2)
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end function

      end module