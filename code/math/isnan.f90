       module isnan_mod
       use current_precision_mod
       implicit none

       private
       public :: my_isnan

       contains

#ifdef _ISNAN_USE_HACK_
       function my_isnan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = isnan(f)
       end function
#else
       function my_isnan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = f.ne.f
       end function
#endif

       end module