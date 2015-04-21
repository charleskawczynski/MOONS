      module IO_transientFields_mod
      use IO_tools_mod
      use IO_tecplotHeaders_mod
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

      public :: writeTransientToFile

      logical,parameter :: headerTecplot = .true.

      interface writeTransientToFile; module procedure writeTransientToFileXYZ; end interface
      interface writeTransientToFile; module procedure writeTransientToFileRes; end interface
        
      contains

      subroutine writeTransientToFileXYZ(x,y,z,n,val,dir,name,firstTime)
        ! Fixes:
        ! Output location then start iterating with time step and 
        ! value, do not repeat location at every time step...
        character(len=*),intent(in) :: dir,name
        real(cp),intent(in) :: x,y,z,val
        logical,intent(in) :: firstTime
        integer,intent(in) :: n
        integer :: u
        if (firstTime) then
          u = newAndOpen(dir,name)
          call writeTecPlotHeader(u,name)
        else
          u = openToAppend(dir,name)
        endif
        write(u,'(5'//arrfmt//')') x,y,z,dble(n),val
        close(u)
      end subroutine

      subroutine writeTransientToFileRes(n,val,dir,name,firstTime)
        character(len=*),intent(in) :: dir,name
        real(cp),intent(in) :: val
        logical,intent(in) :: firstTime
        integer,intent(in) :: n
        integer :: u
        if (firstTime) then
          u = newAndOpen(dir,name)
          call writeTecPlotHeader(u,name)
        else
          u = openToAppend(dir,name)
        endif
        write(u,'(2'//arrfmt//')') dble(n),val
        close(u)
      end subroutine

      end module