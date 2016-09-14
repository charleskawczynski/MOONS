       module coordinate_stretch_parameters_mod
       use current_precision_mod
       implicit none

       private
       public :: RobertsBL
       public :: HartmannBL
       public :: ReynoldsBL

       contains

       function RobertsBL1D(delta,h) result (beta)
         ! RobertsBL1D returns the beta for a given boundary laryer
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

       function RobertsBL(delta,hmin,hmax) result(beta)
         ! RobertsBL returns the beta for a given boundary laryer
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

       function HartmannBL(Ha,hmin,hmax) result (beta)
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: Ha
         real(cp),dimension(3) :: beta
         integer :: i
         real(cp) :: tol
         tol = 10.0_cp**(-10.0_cp)
         if (Ha.lt.tol) then
          stop 'Error: Reynolds number is nearly zero in reynoldsBL in coordinate_stretch_parameters.f90'
         endif
         do i = 1,3
            beta(i) = robertsBL((hmax(i)-hmin(i))/Ha,hmin(i),hmax(i))
         enddo
       end function

       function ReynoldsBL(Re,hmin,hmax) result (beta)
         implicit none
         real(cp),dimension(3),intent(in) :: hmin,hmax
         real(cp),intent(in) :: Re
         real(cp),dimension(3) :: beta
         integer :: i
         real(cp) :: tol
         tol = 10.0_cp**(-10.0_cp)
         if (Re.lt.tol) then
          stop 'Error: Reynolds number is nearly zero in reynoldsBL in coordinate_stretch_parameters.f90'
         endif
         do i=1,3
            beta(i) = robertsBL((hmax(i)-hmin(i))/sqrt(Re),hmin(i),hmax(i))
         enddo
       end function

       end module