       module isnan_mod

       implicit none

       private
       public :: my_isnan

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

       contains

#ifdef _ISNAN_USE_INTRINSIC_
       function my_isnan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = isnan(f)
       end function
#endif

#ifdef _ISNAN_USE_HACK_
       function my_isnan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = f.ne.f
       end function
#endif

       end module