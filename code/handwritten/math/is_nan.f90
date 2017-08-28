       module is_nan_mod
       use current_precision_mod
       implicit none

       private
       public :: is_nan

       contains

#ifdef _ISNAN_USE_HACK_
       function is_nan(f) result(L)
         implicit none
         real(cp),intent(in) :: f
         logical :: L
         L = f.ne.f.or.is_huge(f)
       end function
#else
       function is_nan(f) result(L)
         implicit none
         real(cp),intent(in) :: f
         logical :: L
         L = isnan(f).or.is_huge(f)
       end function
#endif

       function is_huge(f) result(L)
         implicit none
         real(cp),intent(in) :: f
         logical :: L
         L = f.gt.huge(1.0_cp)
       end function

      ! subroutine check_nans_nans(f,caller)
      !   implicit none
      !   real(cp),intent(in) :: f
      !   character(len=*),intent(in) :: caller
      !   if (is_nan(f)) then
      !     write(*,*) 'Error: NaN in ',caller,' in nan.f90'
      !     write(*,*) 'f = ',f
      !     stop 'Done'
      !   endif
      ! end subroutine

       end module