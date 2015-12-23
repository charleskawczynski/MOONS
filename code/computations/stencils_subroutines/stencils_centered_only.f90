      module stencils_mod
      use triDiag_mod
      implicit none
      private
      public :: staggered_assign
      public :: collocated_assign

      public :: staggered_add
      public :: collocated_add

      public :: staggered_subtract
      public :: collocated_subtract

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

      subroutine staggered_assign(dfdh,f,T,s,sdfdh,gt)
        implicit none
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(inout) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        integer,intent(in) :: gt
        integer :: i
        do i=1,s-1
          dfdh(i+gt) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end subroutine
      subroutine collocated_assign(dfdh,f,T,s)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s
        integer :: i
        do i=2,s-1
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      subroutine staggered_add(dfdh,f,T,s,sdfdh,gt)
        implicit none
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(inout) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        integer,intent(in) :: gt
        integer :: i
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) + f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end subroutine
      subroutine collocated_add(dfdh,f,T,s)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s
        integer :: i
        do i=2,s-1
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      subroutine staggered_subtract(dfdh,f,T,s,sdfdh,gt)
        implicit none
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(inout) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        integer,intent(in) :: CC
        integer :: i
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) - (f(i)*T%D(i) + f(i+1)*T%U(i))
        enddo
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end subroutine
      subroutine collocated_subtract(dfdh,f,T,s)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s
        integer :: i
        do i=2,s-1
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      end module