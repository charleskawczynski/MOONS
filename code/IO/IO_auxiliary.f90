      module IO_auxiliary_mod
      use current_precision_mod
      use IO_tools_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

      private
      
      public :: writeSwitchToFile
      public :: readSwitchFromFile
      public :: readLastStepFromFile
      public :: writeLastStepToFile
      public :: writeIntegerToFile
      public :: readIntegerFromFile

      logical,parameter :: headerTecplot = .true.

      contains

      subroutine readLastStepFromFile(n,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(inout) :: n
        integer :: un
        un = openToRead(dir,name)
        read(un,'('//intfmt//')') n
        call closeExisting(un,name,dir)
      end subroutine

      subroutine writeLastStepToFile(n,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: n
        integer :: un
        un = newAndOpen(dir,name)
        write(un,'('//intfmt//')') n
        call closeAndMessage(un,name,dir)
      end subroutine

      subroutine writeSwitchToFile(ks,dir,name)
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: ks
        integer :: un
        un = newAndOpen(dir,name)
        write(un,'('//logfmt//')') ks
        call closeAndMessage(un,name,dir)
      end subroutine

      ! subroutine readSwitchFromFile(ks,dir,name)
      !   character(len=*),intent(in) :: dir,name
      !   logical,intent(inout) :: ks
      !   integer :: un
      !   un = openToRead(dir,name)
      !   read(un,'('//logfmt//')') ks
      !   call closeExisting(un,name,dir)
      ! end subroutine

      function readSwitchFromFile(dir,name) result(ks)
        character(len=*),intent(in) :: dir,name
        logical :: ks
        integer :: un
        un = openToRead(dir,name)
        read(un,'('//logfmt//')') ks
        call closeExisting(un,name,dir)
      end function

      subroutine writeIntegerToFile(i,dir,name)
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: i
        integer :: un
        un = newAndOpen(dir,name)
        write(un,'('//intfmt//')') i
        call closeAndMessage(un,name,dir)
      end subroutine

      function readIntegerFromFile(dir,name) result(val)
        character(len=*),intent(in) :: dir,name
        integer :: val
        integer :: un
        un = openToRead(dir,name)
        read(un,*) val
        call closeExisting(un,name,dir)
      end function

      end module