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
         L = f.ne.f
       end function
#else
       function is_nan(f) result(L)
         implicit none
         real(cp),intent(in) :: f
         logical :: L
         L = isnan(f)
       end function
#endif

       end module