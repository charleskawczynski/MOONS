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

       public :: uniformGrid1,uniformGrid2
       public :: robertsGrid1,robertsGrid2,robertsGrid3
       public :: trim1,trimEnd,reverseIndex
       
       contains

       function uniformGrid1(hmin,hmax,N) result(hn)
         ! This routine returns a uniform grid from
         ! hmin to hmax using N+1 points (N segments).
         ! 
         ! NOTE: hmin and hmax are included in the result.
         ! 
         ! INPUT:
         !      hmin     = minimum value
         !      hmax     = maximum value
         !      N        = N segments of dh
         implicit none
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax
         integer,intent(in) :: N
         integer :: i
         real(cp) :: dh
         dh = (hmax - hmin)/real(N,cp)
         hn = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
       end function

       function uniformGrid2(hstart,dh,N,dir) result(hn)
         ! This routine returns a uniform grid beginning
         ! from hstart with N number of uniform segments dh.
         ! dir is the positive or negative direction.
         ! 
         ! NOTE: hstart is included in the result.
         ! 
         ! INPUT:
         !      hstart   = start value
         !      dh       = step size
         !      N        = number of segments
         !      dir      = (1,-1)
         implicit none
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hstart,dh
         integer,intent(in) :: N,dir
         integer :: i
         hn = (/(hstart+real(dir,cp)*real(i-1,cp)*dh,i=1,N)/)
       end function

       function robertsGrid1(hmin,hmax,N,beta) result(hn)
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

       function robertsGrid2(hmin,hmax,N,alpha,beta) result(hn)
         ! This function returns the coordinates and differences of a Robert's 
         ! stretching function as described in section 5.6 (page 333) of 
         ! Computational Fluid Mechanics and Heat Transfer, 
         ! 2nd edition, J. Tannehill et al. (Transormation 2)
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
         ! NOTE: I have abused notation a bit to provide consistent notation
         ! with the reference as well as generalize the returned grid
         ! so that it need not start at y=0.
         ! 
         implicit none
         integer,intent(in) :: N
         real(cp),dimension(N+1) :: hn
         real(cp),intent(in) :: hmin,hmax,alpha,beta
         real(cp),dimension(:),allocatable :: hnbar
         real(cp) :: dh ! Uniform dh
         integer :: i
         real(cp) :: a,b,c,d,g ! alpha,beta,gamma
         allocate(hnbar(N+1))
         a = alpha; b = beta; c = real(1.0,cp); d = real(2.0,cp)
         g = (b+c)/(b-c)
         ! Where N is the number of cells
         dh = (hmax - hmin)/real(N,cp)
         ! Total coordinates (uniform)
         hnbar = (/(hmin+real(i-1,cp)*dh,i=1,N+1)/)
         ! Push coordinates to zero, and normalize for stretching
         hnbar = (hnbar - hmin)/(hmax-hmin)
         ! Total coordinates (non-uniform Roberts stretching function)
         hn = (/( ((b+d*a)*(g)**((hnbar(i)-a)/(c-a))-b+d*a)/((d*a+c)*(c+&
         g**((hnbar(i)-a)/(c-a)))),i=1,N+1)/)
         ! Return coordinates to original scale:
         hn = hmin + (hmax - hmin)*hn
         deallocate(hnbar)
       end function

       function robertsGrid3(hmin,hmax,N,yc,tau) result(hn)
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

       function robertsGridBL(delta,h) result (beta)
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
         beta = (real(1.0,cp) - delta/h)**(-real(0.5,cp))
       end function

       function robertsGridHa(Ha,h) result (beta)
         ! INPUT:
         !      h        = length of domain
         !      Ha       = Hartmann number
         implicit none
         real(cp),intent(in) :: h,Ha
         real(cp) :: beta
         beta = robertsGridBL(real(1.0,cp)/Ha,h)
       end function

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

       function reverseIndex(h_in,s) result(h_out)
         real(cp),dimension(s),intent(in) :: h_in
         real(cp),dimension(s) :: temp,h_out
         integer :: i,s
         do i=1,s
          temp(s-i+1) = h_in(i)
         enddo
         h_out = temp
       end function

       end module