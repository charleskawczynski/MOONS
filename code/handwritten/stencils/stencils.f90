      module stencils_mod
      use current_precision_mod
      use sparse_mod
      implicit none
      private

      abstract interface
        pure subroutine stencils_stag(dfdh,f,SP,s,sdfdh,gt)
          import :: sparse,cp
          implicit none
          integer,intent(in) :: s,sdfdh
          real(cp),dimension(sdfdh),intent(inout) :: dfdh
          real(cp),dimension(s),intent(in) :: f
          type(sparse),intent(in) :: SP
          integer,intent(in) :: gt
        end subroutine
      end interface

      abstract interface
        pure subroutine stencils_col(dfdh,f,SP,s,pad1,pad2)
          import :: sparse,cp
          implicit none
          integer,intent(in) :: s,pad1,pad2
          real(cp),dimension(s),intent(inout) :: dfdh
          real(cp),dimension(s),intent(in) :: f
          type(sparse),intent(in) :: SP
        end subroutine
      end interface

      public :: stencils_col
      public :: stencils_stag

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

      pure subroutine stag_assign(dfdh,f,SP,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = f(i)*SP%D%f(i) + f(i+1)*SP%U%f(i)
        enddo
      end subroutine
      pure subroutine col_CC_assign(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*SP%L%f(i-1) + &
                    f( i )*SP%D%f(i-1) + &
                    f(i+1)*SP%U%f(i-1)
        enddo

        dfdh(2) = 0.5*(f(1)+f(2))*SP%L%f(1)*(1.0_cp-pad1) + & ! pad1==0
                             f(1)*SP%L%f(1)*pad1 + &          ! pad1==1
                             f(2)*SP%D%f(1) + &
                             f(3)*SP%U%f(1)

        dfdh(s-1) =            f(s-2)*SP%L%f(s-2) + &
                               f(s-1)*SP%D%f(s-2) + &
                    0.5*(f(s-1)+f(s))*SP%U%f(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                 f(s)*SP%U%f(s-2)*pad2               ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      pure subroutine col_N_assign(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = f(i-1)*SP%L%f(i-1) + &
                    f( i )*SP%D%f(i-1) + &
                    f(i+1)*SP%U%f(i-1)
        enddo
        dfdh(2) = f(2-pad1)*SP%L%f(1) + &
                  f(3-pad1)*SP%D%f(1) + &
                  f(4-pad1)*SP%U%f(1)

        dfdh(s-1) = f(s-3+pad2)*SP%L%f(s-2) + &
                    f(s-2+pad2)*SP%D%f(s-2) + &
                    f(s-1+pad2)*SP%U%f(s-2)

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      pure subroutine stag_add(dfdh,f,SP,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) + f(i)*SP%D%f(i) + f(i+1)*SP%U%f(i)
        enddo
      end subroutine
      pure subroutine col_CC_add(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) + f(i-1)*SP%L%f(i-1) + &
                              f( i )*SP%D%f(i-1) + &
                              f(i+1)*SP%U%f(i-1)
        enddo

        dfdh(2) = dfdh(2) + 0.5*(f(1)+f(2))*SP%L%f(1)*(1.0_cp-pad1) + & ! pad1==0
                                       f(1)*SP%L%f(1)*pad1 + &          ! pad1==1
                                       f(2)*SP%D%f(1) + &
                                       f(3)*SP%U%f(1)

        dfdh(s-1) = dfdh(s-1) +          f(s-2)*SP%L%f(s-2) + &
                                         f(s-1)*SP%D%f(s-2) + &
                              0.5*(f(s-1)+f(s))*SP%U%f(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                           f(s)*SP%U%f(s-2)*pad2               ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      pure subroutine col_N_add(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) + f(i-1)*SP%L%f(i-1) + &
                              f( i )*SP%D%f(i-1) + &
                              f(i+1)*SP%U%f(i-1)
        enddo
        dfdh(2) = dfdh(2) + f(2-pad1)*SP%L%f(1) + &
                            f(3-pad1)*SP%D%f(1) + &
                            f(4-pad1)*SP%U%f(1)

        dfdh(s-1) = dfdh(s-1) + f(s-3+pad2)*SP%L%f(s-2) + &
                                f(s-2+pad2)*SP%D%f(s-2) + &
                                f(s-1+pad2)*SP%U%f(s-2)

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      pure subroutine stag_subtract(dfdh,f,SP,s,sdfdh,gt)
        implicit none
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer,intent(in) :: gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp ! Intentially overwritten for gt = 0
        do i=1,s-1
          dfdh(i+gt) = dfdh(i+gt) - (f(i)*SP%D%f(i) + f(i+1)*SP%U%f(i))
        enddo
      end subroutine
      pure subroutine col_CC_subtract(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) - (f(i-1)*SP%L%f(i-1) + &
                               f( i )*SP%D%f(i-1) + &
                               f(i+1)*SP%U%f(i-1))
        enddo

        dfdh(2) = dfdh(2) - (0.5*(f(1)+f(2))*SP%L%f(1)*(1.0_cp-pad1) + & ! pad1==0
                                        f(1)*SP%L%f(1)*pad1 + &          ! pad1==1
                                        f(2)*SP%D%f(1) + &
                                        f(3)*SP%U%f(1))

        dfdh(s-1) = dfdh(s-1) - (           f(s-2)*SP%L%f(s-2) + &
                                            f(s-1)*SP%D%f(s-2) + &
                                 0.5*(f(s-1)+f(s))*SP%U%f(s-2)*(1.0_cp-pad2) + &  ! pad2==0
                                 f(s)*SP%U%f(s-2)*pad2)                           ! pad2==1

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine
      pure subroutine col_N_subtract(dfdh,f,SP,s,pad1,pad2)
        implicit none
        integer,intent(in) :: s,pad1,pad2
        real(cp),dimension(s),intent(inout) :: dfdh
        real(cp),dimension(s),intent(in) :: f
        type(sparse),intent(in) :: SP
        integer :: i
        do i=3,s-2
          dfdh(i) = dfdh(i) - (f(i-1)*SP%L%f(i-1) + &
                               f( i )*SP%D%f(i-1) + &
                               f(i+1)*SP%U%f(i-1))
        enddo
        dfdh(2) = dfdh(2) - (f(2-pad1)*SP%L%f(1) + &
                             f(3-pad1)*SP%D%f(1) + &
                             f(4-pad1)*SP%U%f(1))

        dfdh(s-1) = dfdh(s-1) - (f(s-3+pad2)*SP%L%f(s-2) + &
                                 f(s-2+pad2)*SP%D%f(s-2) + &
                                 f(s-1+pad2)*SP%U%f(s-2))

        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
      end subroutine

      end module