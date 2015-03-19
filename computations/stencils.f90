      module stencils_mod
        ! This module contains all of the stencils used to compute
        ! derivatives on a staggered grid along 1 direction
        ! 
        ! INPUT:
        !     f            = f(:)
        !     dfdh         = dfdhf(:)
        !     dhp          = dh of primary grid data (dh=h2-h1 where df=f2-f1) 
        !     dhd          = dh of dual    grid data (for 2nd derivatives only, dfdh lives on dual grid)
        !     hp           = h of primary  grid data
        !     hd           = h of dual     grid data
        !     s            = size(f)
        !     gt           = 0 :  f {CC}
        !                    1 :  f {N}
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
      !  collocated(f,k,dhp,dhd,hp,hd,s,gt)


#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      interface collocated;    module procedure collocatedDfDh;                  end interface
      interface collocated;    module procedure collocatedD2fDh2;                end interface
      interface collocated;    module procedure collocatedD2fDh2_conservative;   end interface
      ! interface collocated;    module procedure collocatedD2fDh2_conservative2;   end interface

      contains

      function staggered(f,dhp,s,gt) result(dfdh)
        ! This routine computes the 1st derivative of f on the primary
        ! grid. The result lives on the dual grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 0 :  f {CC} , dfdh {N}
        !      1 :  f {N}  , dfdh {CC}
        ! 
        ! NOTE:
        !  f {CC} , dfdh {N}     dfdh = d/dh (f) {everywhere}
        !  f {N}  , dfdh {CC}    dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        implicit none
        real(cp),dimension(:),intent(in) :: f,dhp
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: dfdh
        integer :: i
        dfdh(1) = real(0.0,cp); dfdh(s-1+2*gt) = real(0.0,cp)
        dfdh(1+gt:s-1+gt) = (/((f(i+1)-f(i))/dhp(i),i=1,s-1)/)
      end function

      function collocatedDfDh(f,dhp,s) result(dfdh)
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp
        integer,intent(in) :: s
        integer :: i,j,k
        real(cp) :: alpha,beta
        ! Interior
        k = -1; j = 1
        do i=2,s-1
          alpha = -dhp(i-1); beta = dhp(i)
          dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                    f(i+k)*beta/alpha + &
                    f(i+j)*(-alpha/beta))/(beta-alpha)
        enddo
        ! Forward difference
        i = 1; k = 1; j = 2
        alpha = dhp(1); beta = dhp(1) + dhp(2)
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
        ! Backward difference
        i = s; k = -1; j = -2
        alpha = -dhp(s-1); beta = -(dhp(s-1) + dhp(s-2))
        dfdh(i) = (f( i )*(alpha/beta-beta/alpha) +&
                  f(i+k)*beta/alpha + &
                  f(i+j)*(-alpha/beta))/(beta-alpha)
      end function

      function collocatedD2fDh2(f,dhp,dhd,s,gt) result(dfdh)
        ! This routine computes the 2nd derivative of f on the primary
        ! grid. The result lives on the primary grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 0 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      1 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f
        real(cp),intent(in),dimension(:) :: dhp,dhd
        integer,intent(in) :: s,gt
        integer :: i
        dfdh(1) = real(0.0,cp); dfdh(s) = real(0.0,cp)
        do i=2,s-1
          dfdh(i) = ((f(i+1)-f(i))/dhp(i) - (f(i)-f(i-1))/dhp(i-1))/dhd(i-1+gt)
        enddo
      end function

      function collocatedD2fDh2_conservative(f,k,dhp,dhd,hp,hd,s,gt) result(dfdh)
        ! This routine computes the 2nd derivative of f on the primary
        ! grid. The result lives on the primary grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 0 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      1 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        ! NOTE: This routine assumes that shape(k) = shape(f)
        ! 
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f,k
        real(cp),intent(in),dimension(:) :: dhp,dhd,hp,hd
        integer,intent(in) :: s,gt
        real(cp) :: kpip12,kpim12
        integer :: i
        dfdh(1) = real(0.0,cp); dfdh(s) = real(0.0,cp)
        do i=2,s-1
          kpip12 = (hp(i+1)-hd(i+gt))/(hp(i+1)-hp(i))*(k(i+1) + k(i)) ! interp k to dual grid
          kpim12 = (hp(i)-hd(i-1+gt))/(hp(i)-hp(i-1))*(k(i) + k(i-1)) ! interp k to dual grid

          dfdh(i) = f(i-1)*kpim12/(dhp(i-1)*dhd(i-1+gt)) - &
                    f( i )*(kpim12/(dhp(i-1)*dhd(i-1+gt))+kpip12/(dhp(i)*dhd(i-1+gt))) + &
                    f(i+1)*kpip12/(dhp(i)*dhd(i-1+gt))
        enddo
      end function

      function collocatedD2fDh2_conservative2(f,k,dhp,dhd,hp,hd,s,gt) result(dfdh)
        ! This routine computes the 2nd derivative of f on the primary
        ! grid. The result lives on the primary grid. gt indicates 
        ! whether f lives on the cell center or node of the grid.
        ! 
        ! gt = 0 :  f {CC} , dfdh {N}    (NOT d2fdh2)
        !      1 :  f {N}  , dfdh {CC}   (NOT d2fdh2)
        ! 
        ! NOTE: dfdh = d/dh (f) {interior}, dfdh = 0 {boundary,ghost cells}
        ! 
        ! NOTE: This routine assumes that shape(k) = shape(f)
        ! 
        implicit none
        real(cp),dimension(s) :: dfdh
        real(cp),intent(in),dimension(:) :: f,k
        real(cp),intent(in),dimension(:) :: dhp,dhd,hp,hd
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: temp
        temp = staggered(f,dhp,s,gt)
        temp(1+gt:s-1+2*gt) = temp(1+gt:s-1+2*gt)*interp(k,hp,hd,gt,s-1+2*gt)
        if (gt.eq.0) dfdh = staggered(temp,dhd,size(temp),1)
        if (gt.eq.1) dfdh = staggered(temp,dhd,size(temp),0)
      end function

      function interp(kp,hp,hd,gt,s) result(kd)
        implicit none
        real(cp),dimension(:),intent(in) :: kp,hp,hd
        integer,intent(in) :: s,gt
        real(cp),dimension(s-1+2*gt) :: kd
        integer :: i
        real(cp) :: alpha
        do i=1,size(kp)-1
          alpha = (hp(i+1) - hd(i+gt))/(hp(i+1) - hp(i))
          kd(i+gt) = kp(i)*alpha + kp(i+1)*(real(1.0,cp)-alpha)
        enddo
      end function


      end module