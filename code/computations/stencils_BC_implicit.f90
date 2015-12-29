      module stencils_BC_implicit_mod
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
      ! 
      ! This stencils module adapts finite differenc stencils 
      ! depending on the given boundary conditions

      use triDiag_mod
      use bctype_mod
      implicit none

      private
      public :: stag_CC2N_imp
      public :: stag_N2CC_imp

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

      function stag_CC2N_imp(f,T,s,sdfdh,BCT1,BCT2) result(dfdh)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh) :: dfdh
        type(bctype),intent(in) :: BCT1,BCT2
        integer :: i
        do i=2,s-2
          dfdh(i+1) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        if (BCT1%Dirichlet) dfdh(2)   = (-f(2))*T%D(1) + f(2)*T%U(1)       ! Checked
        if (BCT2%Dirichlet) dfdh(s) = f(s-1)*T%D(s-1) + (-f(s-1))*T%U(s-1) ! Checked
        if (BCT1%Neumann) dfdh(2)   = (f(2))*T%D(1) + f(2)*T%U(1)          ! Checked
        if (BCT2%Neumann) dfdh(s) = f(s-1)*T%D(s-1) + (f(s-1))*T%U(s-1)    ! Checked
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp
      end function

      function stag_N2CC_imp(f,T,s,sdfdh,BCT1,BCT2) result(dfdh)
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,sdfdh
        real(cp),dimension(sdfdh) :: dfdh
        type(bctype),intent(in) :: BCT1,BCT2
        integer :: i
        do i=1,s-1
          dfdh(i) = f(i)*T%D(i) + f(i+1)*T%U(i)
        enddo
        if (BCT1%Neumann) dfdh(1) = (f(3))*T%D(1) + f(2)*T%U(1)                             ! Checked
        if (BCT2%Neumann) dfdh(sdfdh) = f(s-1)*T%D(s-1) + (f(s-2))*T%U(s-1)                 ! Checked
        dfdh(1) = 0.0_cp; dfdh(sdfdh) = 0.0_cp

        if (BCT1%Dirichlet) dfdh(1) = (2.0_cp*f(2)-f(3))*T%D(1) + f(2)*T%U(1)               ! Checked
        if (BCT2%Dirichlet) dfdh(sdfdh) = f(s-1)*T%D(s-1) + (2.0_cp*f(s-1)-f(s-2))*T%U(s-1) ! Checked

        ! if (BCT1%Dirichlet) dfdh(2) = f(3)*T%U(2)
        ! if (BCT2%Dirichlet) dfdh(s-2) = f(s-2)*T%D(s-2)
      end function

      end module