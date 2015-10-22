      module IO_auxiliary_mod
      use IO_tools_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private
      
      public :: writeSwitchToFile
      public :: readSwitchFromFile
      public :: readLastStepFromFile
      public :: writeLastStepToFile

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

      end module