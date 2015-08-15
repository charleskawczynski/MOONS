      module stencils_mod
        ! This module contains all of the stencils used to compute
        ! derivatives on a staggered grid along 1 direction
        ! 
        ! INPUT:
        !     f            = f(:)
        !     dfdh         = dfdhf(:)
        !     dhp          = dh of primary grid data (dh=h2-h1 where df=f2-f1) 
        !     dhd          = dh of dual    grid data (for 2nd derivatives only, dfdh lives on dual grid)
        !     s            = size(f)
        !     gt           = 1 :  f {CC}
        !                    0 :  f {N}
        ! 
        ! INDEXING: The index range of the incoming scalar field is assumed to begin at one.
        !
        ! Insight to these stencils were adopted from
        ! http://wissrech.ins.uni-bonn.de/research/projects/NaSt3DGP/documentation/userguide.pdf
        ! 
        ! CharlieKawczynski@gmail.com

      use triDiag_mod
      implicit none

      private
      public :: staggered, collocated
      !  staggered(f,dhp,s,gt)
      !  collocated(f,dhp,s)
      !  collocated(f,dhp,dhd,s,gt)
      !  collocated(f,k,dhp,dhd,s,gt)


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface staggered;     module procedure staggered_O2;                     end interface
      interface collocated;    module procedure collocated_O2;                  end interface
      interface collocated;    module procedure collocatedD2fDh2;                end interface
      interface collocated;    module procedure collocatedD2fDh2_conservative;   end interface

      ! interface staggered;     module procedure staggered_O4;                     end interface

      contains

      function staggered_O2(f,T,s,gt) result(dfdh)
        ! This routine computes the 1st derivative of f on the primary
        ! grid. The result lives on the dual grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 1 :  f {CC} , dfdh {N}   ,  NOTE: dfdh = d/dh (f) {interior}
        !      0 :  f {N}  , dfdh {CC}  ,  NOTE: dfdh = d/dh (f) {everywhere}
        ! 
        ! NOTE:
        !  f {CC} , dfdh {N}
        !  f {N}  , dfdh {CC}
        ! 
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        ! Interior
        do i=1,s-1
          dfdh(i+gt) = f(i+1)*T%U(i)-f(i)*T%D(i)
        enddo
        ! Boundaries
        dfdh(1) = 0.0_cp; dfdh(s-1+2*gt) = 0.0_cp
      end function

      function staggered_O4(f,dhp,s,gt) result(dfdh)
        ! This routine computes the 1st derivative of f on the primary
        ! grid. The result lives on the dual grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 1 :  f {CC} , dfdh {N}   ,  NOTE: dfdh = d/dh (f) {interior}
        !      0 :  f {N}  , dfdh {CC}  ,  NOTE: dfdh = d/dh (f) {everywhere}
        ! 
        ! NOTE:
        !  f {CC} , dfdh {N}
        !  f {N}  , dfdh {CC}
        ! 
        implicit none
        real(cp),dimension(s),intent(in) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        ! Interior
        do i=1,s-1
          dfdh(i+gt) = f(i-2)*T%L(i-1) + &
                       f(i-1)*T%LL(i-1) + & 
                       f(i)*T%D(i) + &
                       f(i+1)*T%U(i) + &
                       f(i+2)*T%UU(i)
        enddo
        ! Boundaries
        dfdh(1) = 0.0_cp; dfdh(s-1+2*gt) = 0.0_cp
      end function

      function collocated_O2(f,T,s,gt) result(dfdh)
        ! This routine computes the 1st or 2nd derivative (depending
        ! on the triDiag, T) of f on the primary grid. The result 
        ! lives on the primary grid. gt indicates whether f lives on 
        ! the cell center or node of the grid.
        ! gt = 1 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      0 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        implicit none
        real(cp),intent(in),dimension(s) :: f
        type(triDiag),intent(in) :: T
        integer,intent(in) :: gt,s
        real(cp),dimension(s) :: dfdh
        integer :: i
        ! Interior
        do i=3,s-2
          dfdh(i) = f(i-1)*T%L(i-1) + f(i)*T%D(i-1) + f(i+1)*T%U(i-1)
        enddo
        ! Boundaries
        if (gt.eq.1) then
        ! Collocated cell-center derivative, half cell from boundary
        ! Linear interpolation is used to obtain boundary value
        dfdh(2) = 0.5_cp*(f(1)+f(2))*T%L(1) + &
                               f(2) *T%D(1) + &
                               f(3) *T%U(1)
        dfdh(s-1) = 0.5_cp*(f(s)+f(s-1))*T%L(s-2) + &
                                 f(s-1) *T%D(s-2) + &
                                 f( s ) *T%U(s-2)
        else
        ! Collocated cell-corner derivative, on boundary
        dfdh(2) = f(2)*T%D(1) + &
                  f(3)*T%U(1) + &
                  f(4)*T%U(1)
        dfdh(s-1) = f(s-3)*T%L(s-2) + &
                    f(s-2)*T%L(s-2) + &
                    f(s-1)*T%D(s-2)
        endif
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp ! Ghost points
      end function

      function collocatedD2fDh2_conservative(f,k,T1,T2,s,gt) result(dfdh)
        implicit none
        real(cp),intent(in),dimension(s) :: f
        real(cp),dimension(s-1+2*gt) :: k
        type(triDiag),intent(in) :: T1,T2
        integer,intent(in) :: s,gt
        real(cp),dimension(s) :: dfdh
        real(cp),dimension(s-1+2*gt) :: dfdh_temp
        integer :: i
        ! First derivative
        dfdh_temp = staggered(f,T1,s,gt)
        ! Second derivative
        if (gt.eq.1) then; dfdh = staggered(k*dfdh_temp,T2,s-1+2*gt,0)
        else;              dfdh = staggered(k*dfdh_temp,T2,s-1+2*gt,1)
        endif
      end function

      end module