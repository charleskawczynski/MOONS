      module fmt_mod
      implicit none
      ! This website is a good reference for formatting:
      ! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
      ! leading digit + . + precision + E + exponent + signs + spaces between
      !       1         1      12       1      3         2          3
      ! rarrfmt is for reading (possible old formats)
      !  arrfmt is for writing (current format)
      private
      public :: arrfmt,rarrfmt,logfmt,intfmt

      character(len=8),parameter :: rarrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=8),parameter ::  arrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=3),parameter ::  intfmt = 'I15'       ! Make sure length is correct when adjusting
      character(len=3),parameter ::  logfmt = 'L1'        ! Make sure length is correct when adjusting

      ! character(len=8),parameter :: rarr = 'E23.12E3'  ! Make sure length is correct when adjusting
      ! character(len=8),parameter ::  arr = 'E23.12E3'  ! Make sure length is correct when adjusting
      ! character(len=3),parameter ::  int = 'I15'       ! Make sure length is correct when adjusting
      ! character(len=3),parameter ::  log = 'L1'        ! Make sure length is correct when adjusting
      ! character(len=4),parameter :: fileType = '.dat'

      end module