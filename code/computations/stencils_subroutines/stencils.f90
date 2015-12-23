      module stencils_mod
      ! Compiler directives: (_STENCILS_SUPPRESS_WARNING_)
      use triDiag_mod
      implicit none
      private
      public :: stag_assign
      public :: col_CC_assign
      public :: col_N_assign

      public :: stag_add
      public :: col_CC_add
      public :: col_N_add

      public :: stag_subtract
      public :: col_CC_subtract
      public :: col_N_subtract

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

      subroutine stag_assign(dfdh,f,T,s,sdfdh,gt)
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
      subroutine col_CC_assign(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        do i=2,s-1
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
#ifdef _STENCILS_SUPPRESS_WARNING_
        i = pad1; i = pad2
#endif
      end subroutine
      subroutine col_N_assign(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        dfdh(2) = f(2)*T%L(1) + &
                  f(3)*T%D(1) + &
                  f(4)*T%U(1)
        dfdh(s-1) = f(s-3)*T%L(s-2) + &
                    f(s-2)*T%D(s-2) + &
                    f(s-1)*T%U(s-2)
        do i=3-pad1,s-2+pad2
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      subroutine stag_add(dfdh,f,T,s,sdfdh,gt)
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
      subroutine col_CC_add(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        do i=2,s-1
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
#ifdef _STENCILS_SUPPRESS_WARNING_
        i = pad1; i = pad2
#endif
      end subroutine
      subroutine col_N_add(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        dfdh(2) = dfdh(2) + f(2)*T%L(1) + &
                            f(3)*T%D(1) + &
                            f(4)*T%U(1)
        dfdh(s-1) = dfdh(s-1) + f(s-3)*T%L(s-2) + &
                                f(s-2)*T%D(s-2) + &
                                f(s-1)*T%U(s-2)
        do i=3-pad1,s-2+pad2
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      subroutine stag_subtract(dfdh,f,T,s,sdfdh,gt)
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
      subroutine col_CC_subtract(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        do i=2,s-1
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
#ifdef _STENCILS_SUPPRESS_WARNING_
        i = pad1; i = pad2
#endif
      end subroutine
      subroutine col_N_subtract(dfdh,f,T,s,pad1,pad2)
        implicit none
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,pad1,pad2
        integer :: i
        dfdh(2) = dfdh(2) - (f(2)*T%L(1) + &
                             f(3)*T%D(1) + &
                             f(4)*T%U(1))
        dfdh(s-1) = dfdh(s-1) - (f(s-3)*T%L(s-2) + &
                                 f(s-2)*T%D(s-2) + &
                                 f(s-1)*T%U(s-2))
        do i=3-pad1,s-2+pad2
          dfdh(i) = dfdh(i) - (f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1))
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      end module