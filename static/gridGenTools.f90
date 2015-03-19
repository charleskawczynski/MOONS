       module gridGenTools_mod
       implicit none

       private

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       real(cp),parameter :: one = real(1.0,cp)
       real(cp),parameter :: two = real(2.0,cp)
       real(cp),parameter :: zero = real(0.0,cp)
       
       ! Uniform grids
       public :: linspace1,uniformGrid1

       ! Stretched grids
       public :: robertsGrid1,robertsGrid2,robertsGrid3
       public :: robertsGridBL

       ! Other tools
       public :: reverseIndex,trim1,trimEnd

       ! interface robertsGridBL;   module procedure robertsGridBL1D;  end interface
       ! interface robertsGridBL;   module procedure robertsGridBL3D;  end interface

       contains

       ! ***************************************************************
       ! ***************************************************************
       ! *********************** UNIFORM GRIDS *************************
       ! ***************************************************************
       ! ***************************************************************

       subroutine linspace1(hn,hmin,hmax,N)
         ! This routine returns a uniform grid from
         ! hmin to hmax using N+1 points.
         ! 
         ! NOTE: hmin and hmax are included in the result.
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         implicit none
         real(cp),dimension(:),intent(inout) :: hn
         real(cp),intent(in) :: hmin,hmax
         integer,intent(in) :: N
         integer :: i
         real(cp) :: dh
         dh = (hmax - hmin)/real(N,cp)
         hn = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
       end subroutine

       subroutine uniformGrid1(hn,hstart,dh,dir)
         ! This routine returns a uniform grid beginning
         ! from hstart with uniform step size dh.
         ! The size of the segment depends on the size
         ! of hn. dir is the positive or negative direction.
         ! 
         ! NOTE: hstart is included in the result.
         ! 
         ! INPUT:
         !      hstart   = start value
         !      dh       = step size
         !      N        = N points in segment
         !      dir      = (1,-1)
         implicit none
         real(cp),dimension(:),intent(inout) :: hn
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: dir
         integer :: s,i
         s = size(hn)
         ! Total coordinates (uniform)
         hn = (/(hstart+real(dir,cp)*real(i-1,cp)*dh,i=1,s)/)
       end subroutine

       ! ***************************************************************
       ! ***************************************************************
       ! ********************* STRETCHED GRIDS *************************
       ! ***************************************************************
       ! ***************************************************************

       function robertsGrid1(hmin,hmax,N,beta) result(hn)
         ! NOT YET TESTED
         ! 
         ! This function returns the coordinates and differences of a Robert's 
         ! stretching function as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al. (Transormation 1)
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         !      beta     = 1.0 <= beta <= infinity = stretching factor
         !               = larger -> less stretching
         ! 
         !    y=0                         y=h
         !                                 |
         !     |-|--|---|-------|----------|
         !     |------> y
         ! 
         ! NOTE: I have abused notation a bit to provide consistent notation
         ! with the reference as well as generalize the returned grid
         ! so that it need not start at y=0.
         ! 
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,beta
         real(cp),dimension(:),allocatable :: hnbar
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,b,g ! beta,gamma
         allocate(hnbar(N+1))
         a = real(1.0,cp); b = beta
         g = (b+real(1.0,cp))/(b-real(1.0,cp))
         ! Where N is the number of cells
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( ((b+a)-(b-a)*g**(a-hnbar))/(g**(a-hnbar)+a),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end function

       subroutine robertsGrid2(hn,hmin,hmax,N,alpha,beta)
         ! This function returns the coordinates and differences of a Robert's 
         ! stretching function as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al.
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         !      alpha    = 0      stretching at y=h only
         !      alpha    = 0.5    stretching at y=0 and y=hmax
         !      beta     = 1.0 <= beta <= infinity = stretching factor
         !               = larger -> less stretching
         ! 
         ! Here is a picture illustration for alpha = 0:
         ! 
         !                                y=h
         !                                 |
         !     |----------|-------|---|--|-|
         !     |------> y
         ! 
         ! Note that this must be used in reverse for the lid driven
         ! cavity geometry for the 'front' and 'back' walls.
         ! 
         ! NOTE: I have abused notation a bit to provide consistent notation
         ! with the reference as well as generalize the returned grid
         ! so that it need not start at y=0.
         ! 
         implicit none
         real(cp),dimension(:),intent(inout) :: hn
         real(cp),intent(in) :: hmin,hmax,alpha,beta
         integer,intent(in) :: N
         real(cp),dimension(:),allocatable :: hnbar
         integer :: s
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,b,g ! alpha,beta,gamma
         s = size(hn)
         allocate(hnbar(s))
         a = alpha; b = beta
         g = (b+one)/(b-one)
         ! Where N is the number of cells in the wall
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( ((b+two*a)*(g)**((hnbar(i)-a)/(one-a))-&
         b+two*a)/((two*a+one)*(one+&
         g**((hnbar(i)-a)/(one-a)))),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end subroutine

       function robertsGrid3(hmin,hmax,N,yc,tau) result(hn)
         ! NOT YET TESTED
         ! 
         ! This function returns the coordinates and differences of a Robert's 
         ! stretching function as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al. (Transormation 3)
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         !      tau      = 0.0 <= tau <= infinity = stretching factor
         ! 
         !      tau = 0            --> no stretching
         !      tau = large values --> strong stretching
         ! 
         ! Here is a picture illustration for alpha = 0:
         ! 
         !                                y=yc                        y=h
         !                                 |
         !     |----------|-------|---|--|-|-|--|---|-------|----------|
         !     |------> y
         ! 
         ! Note that this must be used in reverse for the lid driven
         ! cavity geometry for the 'front' and 'back' walls.
         ! 
         ! NOTE: I have abused notation a bit to provide consistent notation
         ! with the reference as well as generalize the returned grid
         ! so that it need not start at y=0.
         ! 
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,yc,tau
         real(cp),dimension(:),allocatable :: hnbar
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,B,c,d,e
         allocate(hnbar(N+1))
         a = real(1.0,cp); c = real(2.0,cp)
         d = yc/(hmax-hmin); e = real(exp(tau),cp)
         B = a/(c*tau)*log((a+(e-a)*d)/(a+(a/e-a)*d))
         ! Where N is the number of cells
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( yc*(a+real(sinh(tau*(hnbar(i)-B)),cp)/real(sinh(tau*B),cp)),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end function

       subroutine robertsGridBL(beta,delta,hmin,hmax)
         ! robertsGridBL returns the beta for a given boundary laryer
         ! as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al.
         ! 
         ! INPUT:
         !      hmin     = wall boundary (minimum value)
         !      hmax     = wall boundary (maximum value)
         !      delta    = thickness of boundary layer
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: delta
         real(cp),dimension(3),intent(inout) :: beta
         integer :: i
         do i=1,3
           beta(i) = (one - delta/(hmax(i)-hmin(i)))**(-one/two)
         enddo
       end subroutine

       function robertsGridBL1D(delta,h) result (beta)
         ! robertsGridBL returns the beta for a given boundary laryer
         ! as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al.
         ! 
         ! INPUT:
         !      hmin     = wall boundary (minimum value)
         !      hmax     = wall boundary (maximum value)
         !      delta    = thickness of boundary layer
         implicit none
         real(cp),intent(in) :: h,delta
         real(cp) :: beta
         ! This if statement protects against the case when 
         ! Ha = 1, which leads to beta = infinity. HIMAG
         ! doesn't seem to protect against this.
         if (delta.lt.h*real(0.99,cp)) then
           beta = (real(1.0,cp) - delta/h)**(-real(0.5,cp))
         else; beta = real(1000.0,cp)
         endif
       end function

       function robertsGridBL3D(delta,h) result (beta)
         implicit none
         real(cp),dimension(3),intent(in) :: h
         real(cp),intent(in) :: delta
         real(cp),dimension(3) :: beta
         integer :: i
         do i = 1,3
            beta(i) = robertsGridBL1D(delta,h(i))
         enddo
       end function

       ! ***************************************************************
       ! ***************************************************************
       ! *********************** OTHER TOOLS ***************************
       ! ***************************************************************
       ! ***************************************************************

       subroutine reverseIndex(h)
         real(cp),dimension(:),intent(inout) :: h
         real(cp),dimension(:),allocatable :: temp
         integer :: i,s
         s = size(h)
         allocate(temp(s))
         do i=1,s
          temp(s-i+1) = h(i)
         enddo
         h = temp
         deallocate(temp)
       end subroutine

       function trim1(h_in,s) result(h_out)
         implicit none
         integer,intent(in) :: s
         real(cp),dimension(s),intent(in) :: h_in
         real(cp),dimension(s-1) :: h_out
         h_out = h_in(2:s)
       end function

       function trimEnd(h_in,s) result(h_out)
         implicit none
         integer,intent(in) :: s
         real(cp),dimension(s),intent(in) :: h_in
         real(cp),dimension(s-1) :: h_out
         h_out = h_in(1:s-1)
       end function


       end module