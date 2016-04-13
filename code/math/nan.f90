       module isnan_mod

       implicit none

       private
       public :: isnan

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

       function isnan(f) result(TF)
         implicit none
         real(cp),intent(in) :: f
         logical :: TF
         TF = 
       end function

       end module