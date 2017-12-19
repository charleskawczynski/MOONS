      module current_precision_mod
      implicit none

      private
      public :: cp,li
      public :: pow
      public :: equal
      public :: machine_epsilon

#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif

       real(cp),parameter :: machine_epsilon = epsilon(1.0_cp)
       integer,parameter :: li = selected_int_kind(16)
       ! integer,parameter :: ip = selected_int_kind(8)  ! Short int

       ! integer,parameter :: cip = selected_int_kind(64)

       contains

       pure function pow(i) result(p)
         implicit none
         integer,intent(in) :: i
         real(cp) :: p
         p = 10.0_cp**(real(i,cp))
       end function

       pure function equal(A,B) result(L)
         implicit none
         real(cp),intent(in) :: A,B
         real(cp) :: tol
         logical :: L
         tol = 100.0_cp*machine_epsilon
         L = abs(A-B).lt.tol
       end function

      end module