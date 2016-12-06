      module current_precision_mod
      implicit none

      private
      public :: cp,li

#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif

       integer,parameter :: li = selected_int_kind(16)
       ! integer,parameter :: ip = selected_int_kind(8)  ! Short int

       ! integer,parameter :: cip = selected_int_kind(64)

      end module