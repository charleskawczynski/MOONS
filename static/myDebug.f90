      module myDebug_mod
      use constants_mod
      implicit none

     ! Fixes / Improvements:

      logical,parameter :: debugOn = .true.

      contains

      subroutine debug(i)
       implicit none
       integer,intent(in) :: i
       if (debugOn) then
         write(*,*) 'The program has reached this location. ',i
!        pause
       endif
      end subroutine

      end module