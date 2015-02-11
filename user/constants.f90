      module constants_mod
      implicit none

        integer,parameter :: spn = selected_real_kind(8)
        integer,parameter :: dpn = selected_real_kind(14)
        integer,parameter :: qpn = selected_real_kind(32)

        integer,parameter :: SRK8 = selected_real_kind(8)
        integer,parameter :: SRK14 = selected_real_kind(14)
        integer,parameter :: SRK32 = selected_real_kind(32)

        real(dpn),parameter :: PI = 3.14159265358979
        real(dpn),parameter :: zero = 0.0
        real(dpn),parameter :: oneHalf = 0.5
        real(dpn),parameter :: one = 1.0
        real(dpn),parameter :: two = 2.0
        real(dpn),parameter :: three = 3.0
        real(dpn),parameter :: REAL_MIN = tiny(1.0)
        real(dpn),parameter :: REAL_MAX = huge(1.0)
        real(dpn),parameter :: RAD_TO_DEG = 180.0/PI
        real(dpn),parameter :: DEG_TO_RAD = PI/180.0
        logical,parameter :: true = .true.
        logical,parameter :: false = .false.

      contains

      subroutine myPause()
      implicit none
        ! pause
      end subroutine

      end module