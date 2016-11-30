       module coordinate_distribution_funcs_mod
       use current_precision_mod
       use coordinate_stretch_parameters_mod
       implicit none

       private
       public :: uniform,linspace,uniformLeft,uniformRight ! Uniform grids
       public :: robertsLeft,robertsRight,robertsBoth,cluster ! Stretched grids

       real(cp),parameter :: one = 1.0_cp
       real(cp),parameter :: two = 2.0_cp
       real(cp),parameter :: zero = 0.0_cp
       real(cp),parameter :: tol = 10.0_cp**(-12.0_cp)

       contains

       ! ***************************************************************
       ! *********************** UNIFORM GRIDS *************************
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
         if (N.lt.0) stop 'Error: N < 0 in uniform in grid_distribution_funcs.f90'
         if (N.eq.0) then
           if (abs(hmax-hmin).lt.tol) then
             dh = 0.0_cp
             hn = hmin
             else; stop 'Error: N = 0 but hmin.ne.hmax in grid_distribution_funcs.f90'
           endif
         else
           dh = (hmax - hmin)/real(N,cp)
           hn = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         endif
       end function

       subroutine linspace(hn,hmin,hmax,N)
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
         real(cp),dimension(N+1),intent(inout) :: hn
         real(cp),intent(in) :: hmin,hmax
         integer,intent(in) :: N
         integer :: i
         real(cp) :: dh
         dh = (hmax - hmin)/real(N,cp)
         hn = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
       end subroutine

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
         if (N.lt.0) stop 'Error: N < 0 in uniformDirection in grid_distribution_funcs.f90'
         if (N.eq.0) then
           if (dh.lt.tol) then
             hn = hstart
           else; stop 'Error: N = 0 but dh.gt.tol in uniformDirection in grid_distribution_funcs.f90'
           endif
         else
           hn = (/(hstart+real(dir,cp)*real(i-1,cp)*dh,i=1,N+1)/)
           if (dir.eq.-1) call reverseIndex(hn,N+1)
         endif
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
       ! ********************* STRETCHED GRIDS *************************
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
         !      tau      = 0.0_cp <= tau <= infinity = stretching factor
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
       ! ************************* ALIASES *****************************
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
       ! *********************** OTHER TOOLS ***************************
       ! ***************************************************************

       subroutine reverseIndex(h,N)
         integer,intent(in) :: N
         real(cp),dimension(N),intent(inout) :: h
         real(cp),dimension(:),allocatable :: temp
         integer :: i
         allocate(temp(N))
         do i=1,N
          temp(N-i+1) = h(i)
         enddo
         h = temp
         deallocate(temp)
       end subroutine

       ! subroutine reverseIndex_new(h,N) ! Probably the same, but faster than above
       !   real(cp),dimension(N),intent(inout) :: h
       !   h = h(N:1:-1)
       ! end subroutine

       end module