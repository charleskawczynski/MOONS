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

      interface staggered;     module procedure staggeredO2;                     end interface
      ! interface staggered;     module procedure staggeredO4;                     end interface
      interface collocated;    module procedure collocatedDfDh;                  end interface
      interface collocated;    module procedure collocatedD2fDh2;                end interface
      interface collocated;    module procedure collocatedD2fDh2_conservative;   end interface

      contains

      function staggeredO2(f,dhp,s,gt) result(dfdh)
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
        real(cp),dimension(:),intent(in) :: f,dhp
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(s-1+2*gt) = 0.0_cp
        dfdh(1+gt:s-1+gt) = (/((f(i+1)-f(i))/dhp(i),i=1,s-1)/)
      end function

      function staggeredO4(f,dhp,s,gt) result(dfdh)
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
        ! THIS IS FOR UNIFORM GRIDS ONLY. 2nd ORDER ACCURACY IS
        ! USED NEAR THE BOUNDARIES
        implicit none
        real(cp),dimension(:),intent(in) :: f,dhp
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(s-1+2*gt) = 0.0_cp

        dfdh(2+gt:s-2+gt) = (/((+f(i-1) &
                  -27.0_cp*f(i)&
                  +27.0_cp*f(i+1)&
                                -f(i+2) &
                                )/(24.0_cp*dhp(i)),i=2,s-2)/)
        dfdh(1+gt) = (f(2)-f(1))/dhp(1)
        dfdh(s-1+gt) = (f(s)-f(s-1))/dhp(s-1)
      end function

      function collocatedDfDh(f,dhp,s,gt) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp
        integer,intent(in) :: gt,s
        integer :: i,j,k
        real(cp) :: alpha,beta,temp_b
        ! Interior
        k = -1; j = 1
        do i=2,s-1
          alpha = -dhp(i-1); beta = dhp(i)
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)
        enddo
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
        if (gt.eq.1) then            ! Collocated CellCenter derivative
        ! Forward difference
        k = -1; j = 1
        temp_b = 0.5_cp*(f(1)+f(2)) ! Linear interpolate to boundary
        i = 2
        alpha = -0.5_cp*dhp(i-1); beta = dhp(i)
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  temp_b*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
        ! Backward difference
        k = -1; j = 1
        temp_b = 0.5_cp*(f(s)+f(s-1)) ! Linear interpolate to boundary
        i = s-1
        alpha = -dhp(i-1); beta = 0.5_cp*dhp(i)
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  temp_b*(-alpha/beta))/(beta-alpha)
        else                         ! Collocated Node derivative
        ! Forward difference
        k = 1; j = 2
        temp_b = 0.5_cp*(f(1)+f(2)) ! Linear interpolate to boundary
        i = 2
        alpha = dhp(i); beta = dhp(i)+dhp(i+1)
        dfdh(i) = (temp_b*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
        ! Backward difference
        k = -2; j = -1
        temp_b = 0.5_cp*(f(s)+f(s-1)) ! Linear interpolate to boundary
        i = s-1
        alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2))
        dfdh(i) = (temp_b*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
        endif
      end function

      function collocatedD2fDh2(f,dhp,dhd,s,gt) result(dfdh)
        ! This routine computes the 2nd derivative of f on the primary
        ! grid. The result lives on the primary grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 1 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      0 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s,gt
        integer :: i
        integer :: j,k
        real(cp) :: temp_b,alpha,beta
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
        do i=2,s-1
          dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1+gt)
        enddo

        if (gt.eq.1) then              ! Collocated CellCenter derivative (gt = 1):
          ! Forward difference
          k = -1; j = 1
          temp_b = 0.5_cp*(f(1)+f(2)) ! Linear interpolate to boundary
          i = 2
          alpha = -0.5_cp*dhp(i-1); beta = dhp(i)
          dfdh(i) = 2.0_cp*f(i)/(alpha*beta) + &
                    2.0_cp*temp_b/(alpha**2.0_cp - alpha*beta) + &
                    2.0_cp*f(i+j)/(beta**2.0_cp - alpha*beta)
          ! Backward difference
          k = -1; j = 1
          temp_b = 0.5_cp*(f(s)+f(s-1)) ! Linear interpolate to boundary
          i = s-1
          alpha = -dhp(i-1); beta = 0.5_cp*dhp(i)
          dfdh(i) = 2.0_cp*f(i)/(alpha*beta) + &
                    2.0_cp*f(i+k)/(alpha**2.0_cp - alpha*beta) + &
                    2.0_cp*temp_b/(beta**2.0_cp - alpha*beta)
        else                           ! Collocated Node derivative (gt = 0)
          ! Forward difference
          k = 1; j = 2
          temp_b = 0.5_cp*(f(1)+f(2)) ! Linear interpolate to boundary
          i = 2
          alpha = dhp(i); beta = dhp(i)+dhp(i+1)
          dfdh(i) = 2.0_cp*temp_b/(alpha*beta) + &
                    2.0_cp*f(i+k)/(alpha**2.0_cp - alpha*beta) + &
                    2.0_cp*f(i+j)/(beta**2.0_cp - alpha*beta)
          ! Backward difference
          k = -2; j = -1
          temp_b = 0.5_cp*(f(s)+f(s-1)) ! Linear interpolate to boundary
          i = s-1
          alpha = -dhp(i-1)-dhp(i-2); beta = -dhp(i-1)
          dfdh(i) = 2.0_cp*temp_b/(alpha*beta) + &
                    2.0_cp*f(i+k)/(alpha**2.0_cp - alpha*beta) + &
                    2.0_cp*f(i+j)/(beta**2.0_cp - alpha*beta)
        endif
      end function

      function collocatedD2fDh2_conservative(f,k,dhp,dhd,s,gt) result(dfdh)
        ! This routine computes the 2nd derivative of f on the primary
        ! grid. The result lives on the primary grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 1 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      0 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        ! NOTE: This routine assumes that k is 
        ! 
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f,k
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s,gt
        integer :: i
        dfdh(1) = 0.0_cp; dfdh(s) = 0.0_cp
        do i=2,s-1
          dfdh(i) = f(i-1)*k(i-1+gt)/(dhp(i-1)*dhd(i-1+gt)) - &
                    f( i )*(k(i-1+gt)/(dhp(i-1)*dhd(i-1+gt))+k(i+gt)/(dhp(i)*dhd(i-1+gt))) + &
                    f(i+1)*k(i+gt)/(dhp(i)*dhd(i-1+gt))
        enddo
      end function

      end module