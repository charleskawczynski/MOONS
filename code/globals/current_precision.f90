      module current_precision_mod
      implicit none

      private
      public :: cp,li

#ifdef _QUAD_PRECISION_
       ! integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       ! integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif

       integer,parameter :: li = selected_int_kind(16)
       ! integer,parameter :: ip = selected_int_kind(8)  ! Short int

       ! integer,parameter :: cip = selected_int_kind(64)
       ! real(cp),parameter :: PI = 4.0_cp*atan(1.0_cp)
       ! real(cp),parameter :: PI = 3.141592653589793238462643383279502884197169399375105820974_cp

      end module