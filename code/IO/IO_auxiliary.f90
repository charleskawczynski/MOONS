      module IO_auxiliary_mod
      use current_precision_mod
      use IO_tools_mod
      use fmt_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

      private
      
      ! public :: writeSwitchToFile
      ! public :: readSwitchFromFile
      ! public :: readLastStepFromFile
      ! public :: writeLastStepToFile
      ! public :: writeIntegerToFile
      ! public :: readIntegerFromFile

      logical,parameter :: headerTecplot = .true.

      contains

      subroutine readLastStepFromFile(n,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(inout) :: n
        integer :: un
        un = open_to_read(dir,name)
        read(un,'('//intfmt//')') n
        call close_and_message(un,dir,name)
      end subroutine

      subroutine writeLastStepToFile(n,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: n
        integer :: un
        un = new_and_open(dir,name)
        write(un,'('//intfmt//')') n
        call close_and_message(un,dir,name)
      end subroutine

      subroutine writeSwitchToFile(ks,dir,name)
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: ks
        integer :: un
        un = new_and_open(dir,name)
        write(un,'('//logfmt//')') ks
        call close_and_message(un,dir,name)
      end subroutine

      function readSwitchFromFile(dir,name) result(ks)
        character(len=*),intent(in) :: dir,name
        logical :: ks
        integer :: un
        un = open_to_read(dir,name)
        read(un,'('//logfmt//')') ks
        call close_and_message(un,dir,name)
      end function

      subroutine writeIntegerToFile(i,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: i
        integer :: un
        un = new_and_open(dir,name)
        write(un,'('//intfmt//')') i
        call close_and_message(un,dir,name)
      end subroutine

      function readIntegerFromFile(dir,name) result(val)
        character(len=*),intent(in) :: dir,name
        integer :: val
        integer :: un
        un = open_to_read(dir,name)
        read(un,*) val
        call close_and_message(un,dir,name)
      end function

      end module