       module is_nan_mod
       use current_precision_mod
       implicit none

       private
       public :: is_nan

       contains

#ifdef _ISNAN_USE_HACK_
       function is_nan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = f.ne.f
       end function
#else
       function is_nan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = isnan(f)
       end function
#endif

       end module