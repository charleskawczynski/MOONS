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

       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: two = 2.0_cp
       real(cp),parameter :: zero = 0.0_cp
       
       ! Uniform grids
       public :: uniform,uniformLeft,uniformRight
       public :: linspace1,uniformGrid1

       ! Stretched grids
       public :: robertsLeft,robertsRight,robertsBoth,cluster
       ! Stretching parameters
       public :: robertsBL
       ! Physical stretching arameters
       public :: hartmannBL,reynoldsBL

       ! Other tools
       public :: robertsGrid2                   ! Soon to be private
       public :: reverseIndex,defineGhostPoints ! Soon to be private

       contains

       ! ***************************************************************
       ! ***************************************************************
       ! *********************** UNIFORM GRIDS *************************
       ! ***************************************************************
       ! ***************************************************************

       function uniform(hmin,hmax,N) result(hn)
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
         real(cp),intent(in) :: hmin,hmax
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         integer :: i
         real(cp) :: dh
         dh = (hmax - hmin)/real(N,cp)
         hn = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
       end function

       function uniformDirection(hstart,dh,N,dir) result(hn)
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
         !      N        = N segments
         !      dir      = (1,-1)
         implicit none
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: N,dir
         real(cp),dimension(N+1) :: hn
         integer :: i
         ! Total coordinates (uniform)
         hn = (/(hstart+real(dir,cp)*real(i-1,cp)*dh,i=1,N+1)/)
         if (dir.eq.-1) call reverseIndex(hn)
       end function

       function uniformLeft(hstart,dh,N) result(hn)
         ! Uses uniformDirection. Output:
         ! 
         !                     |
         !    --|--|--|--|--|--|
         !                     |
         !                   hstart
         implicit none
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         hn = uniformDirection(hstart,dh,N,-1)
       end function

       function uniformRight(hstart,dh,N) result(hn)
         ! Uses uniformDirection. Output:
         ! 
         !                     |
         !                     |--|--|--|--|--|--
         !                     |
         !                   hstart
         implicit none
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         hn = uniformDirection(hstart,dh,N,1)
       end function

       ! ***************************************************************
       ! ***************************************************************
       ! ********************* STRETCHED GRIDS *************************
       ! ***************************************************************
       ! ***************************************************************

       function transformation1(hmin,hmax,N,beta) result(hn)
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
         a = 1.0_cp; b = beta
         g = (b+1.0_cp)/(b-1.0_cp)
         ! Where N is the number of cells
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( ((b+a)-(b-a)*g**(a-hnbar(i)))/(g**(a-hnbar(i))+a),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end function

       function transformation2(hmin,hmax,N,alpha,beta) result(hn)
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
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,alpha,beta
         real(cp),dimension(:),allocatable :: hnbar
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,b,g ! alpha,beta,gamma
         allocate(hnbar(N+1))
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
       end function

       function transformation3(hmin,hmax,N,yc,tau) result(hn)
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
         a = 1.0_cp; c = 2.0_cp
         ! Where N is the number of cells
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Define stretching parameters
         d = yc/(hmax-hmin); e = real(exp(tau),cp)
         B = a/(c*tau)*log((a+(e-a)*d)/(a+(a/e-a)*d))
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( d*(a+real(sinh(tau*(hnbar(i)-B)),cp)/real(sinh(tau*B),cp)),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end function

       ! ***************************************************************
       ! ***************************************************************
       ! ************************* ALIASES *****************************
       ! ***************************************************************
       ! ***************************************************************

       function robertsLeft(hmin,hmax,N,beta) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,beta
         hn = transformation1(hmin,hmax,N,beta)
       end function

       function robertsRight(hmin,hmax,N,beta) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,beta
         hn = transformation2(hmin,hmax,N,0.0_cp,beta)
       end function

       function robertsBoth(hmin,hmax,N,beta) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,beta
         hn = transformation2(hmin,hmax,N,0.5_cp,beta)
       end function

       function cluster(hmin,hmax,N,yc,tau) result(hn)
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,yc,tau
         hn = transformation3(hmin,hmax,N,yc,tau)
       end function

       ! ***************************************************************
       ! ***************************************************************
       ! ************************* PHYSICAL ****************************
       ! ***************************************************************
       ! ***************************************************************

       function robertsBL1D(delta,h) result (beta)
         ! robertsBL1D returns the beta for a given boundary laryer
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
         ! The 'if' statement protects against the case when 
         ! Ha = 1 (delta=h), which leads to beta = infinity. 
         ! HIMAG doesn't seem to protect against this.
         if (delta.lt.h*0.99_cp) then
           beta = (1.0_cp - delta/h)**(-0.5_cp)
         else; beta = 1000.0_cp
         endif
       end function

       function robertsBL(delta,hmin,hmax) result(beta)
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
         real(cp),intent(in) :: hmin,hmax
         real(cp),intent(in) :: delta
         real(cp) :: beta
         beta = robertsBL1D(delta,hmax-hmin)
       end function

       function hartmannBL(Ha,hmin,hmax) result (beta)
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: Ha
         real(cp),dimension(3) :: beta
         integer :: i
         do i = 1,3
            beta(i) = robertsBL((hmax(i)-hmin(i))/Ha,hmin(i),hmax(i))
         enddo
       end function

       function reynoldsBL(Re,hmin,hmax) result (beta)
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: Re
         real(cp),dimension(3) :: beta
         integer :: i
         do i = 1,3
            beta(i) = robertsBL((hmax(i)-hmin(i))/sqrt(Re),hmin(i),hmax(i))
         enddo
       end function

       ! ***************************************************************
       ! ***************************************************************
       ! *********************** OTHER TOOLS ***************************
       ! ***************************************************************
       ! ***************************************************************
       ! These routines must be removed with the addition of gridGen.

       subroutine robertsGrid2(hn,hmin,hmax,N,alpha,beta) ! OBSOLETE, NEED TO DELETE
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

       subroutine reverseIndex_new(h)
         real(cp),dimension(:),intent(inout) :: h
         h = h(size(h):1:-1)
       end subroutine

       subroutine defineGhostPoints(h)
         implicit none
         real(cp),dimension(:),intent(inout) :: h
         integer :: s
         s = size(h)
         h(1) = h(2) - (h(3) - h(2))
         h(s) = h(s-1) + (h(s-1) - h(s-2))
       end subroutine

       end module