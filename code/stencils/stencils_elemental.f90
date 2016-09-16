      module stencils_mod
      use current_precision_mod
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

      contains

      elemental subroutine stag_assign(dfdh,f,T,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
      end subroutine
      elemental subroutine col_CC_assign(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + &
                    f( i )*T%D(i-1) + &
                    f(i+1)*T%U(i-1)
        enddo

        dfdh(2) = 0.5*(f(1)+f(2))*T%L(1)*(1.0_cp-pad1) + & ! pad1==0
                             f(1)*T%L(1)*pad1 + &          ! pad1==1
                             f(2)*T%D(1) + &
                             f(3)*T%U(1)

        dfdh(s-1) =            f(s-2)*T%L(s-2) + &
                               f(s-1)*T%D(s-2) + &
                    0.5*(f(s-1)+f(s))*T%U(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                 f(s)*T%U(s-2)*pad2               ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      elemental subroutine col_N_assign(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + &
                    f( i )*T%D(i-1) + &
                    f(i+1)*T%U(i-1)
        enddo
        dfdh(2) = f(2-pad1)*T%L(2-1) + &
                  f(3-pad1)*T%D(2-1) + &
                  f(4-pad1)*T%U(2-1)

        dfdh(s-1) = f(s-3+pad2)*T%L(s-2) + &
                    f(s-2+pad2)*T%D(s-2) + &
                    f(s-1+pad2)*T%U(s-2)

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      elemental subroutine stag_add(dfdh,f,T,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) + f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
      end subroutine
      elemental subroutine col_CC_add(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + &
                              f( i )*T%D(i-1) + &
                              f(i+1)*T%U(i-1)
        enddo

        dfdh(2) = dfdh(2) + 0.5*(f(1)+f(2))*T%L(1)*(1.0_cp-pad1) + & ! pad1==0
                                       f(1)*T%L(1)*pad1 + &          ! pad1==1
                                       f(2)*T%D(1) + &
                                       f(3)*T%U(1)

        dfdh(s-1) = dfdh(s-1) +          f(s-2)*T%L(s-2) + &
                                         f(s-1)*T%D(s-2) + &
                              0.5*(f(s-1)+f(s))*T%U(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                           f(s)*T%U(s-2)*pad2               ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      elemental subroutine col_N_add(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) + f(i-1)*T%L(i-1) + &
                              f( i )*T%D(i-1) + &
                              f(i+1)*T%U(i-1)
        enddo
        dfdh(2) = dfdh(2) + f(2-pad1)*T%L(2-1) + &
                            f(3-pad1)*T%D(2-1) + &
                            f(4-pad1)*T%U(2-1)

        dfdh(s-1) = dfdh(s-1) + f(s-3+pad2)*T%L(s-2) + &
                                f(s-2+pad2)*T%D(s-2) + &
                                f(s-1+pad2)*T%U(s-2)

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      elemental subroutine stag_subtract(dfdh,f,T,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) - (f(i)*T%D(i) + f(i+1)*T%U(i))
        enddo
      end subroutine
      elemental subroutine col_CC_subtract(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) - (f(i-1)*T%L(i-1) + &
                               f( i )*T%D(i-1) + &
                               f(i+1)*T%U(i-1))
        enddo

        dfdh(2) = dfdh(2) - (0.5*(f(1)+f(2))*T%L(1)*(1.0_cp-pad1) + & ! pad1==0
                                        f(1)*T%L(1)*pad1 + &          ! pad1==1
                                        f(2)*T%D(1) + &
                                        f(3)*T%U(1))

        dfdh(s-1) = dfdh(s-1) - (           f(s-2)*T%L(s-2) + &
                                            f(s-1)*T%D(s-2) + &
                                 0.5*(f(s-1)+f(s))*T%U(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                 f(s)*T%U(s-2)*pad2)                           ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      elemental subroutine col_N_subtract(dfdh,f,T,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) - (f(i-1)*T%L(i-1) + &
                               f( i )*T%D(i-1) + &
                               f(i+1)*T%U(i-1))
        enddo
        dfdh(2) = dfdh(2) - (f(2-pad1)*T%L(2-1) + &
                             f(3-pad1)*T%D(2-1) + &
                             f(4-pad1)*T%U(2-1))

        dfdh(s-1) = dfdh(s-1) - (f(s-3+pad2)*T%L(s-2) + &
                                 f(s-2+pad2)*T%D(s-2) + &
                                 f(s-1+pad2)*T%U(s-2))

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      end module