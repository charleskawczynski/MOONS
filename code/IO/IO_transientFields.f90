      module IO_transientFields_mod
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

      public :: writeTransientToFile

      logical,parameter :: headerTecplot = .true.

      ! interface writeTransientToFile; module procedure writeTransientToFileXYZ;      end interface
      interface writeTransientToFile; module procedure writeTransientToFileRes;      end interface
        
      contains

      ! subroutine writeTransientToFileXYZ(x,y,z,n,val,dir,name,firstTime)
      !   ! Fixes:
      !   ! Output location then start iterating with time step and 
      !   ! value, do not repeat location at every time step...
      !   character(len=*),intent(in) :: dir,name
      !   real(cp),intent(in) :: x,y,z,val
      !   logical,intent(in) :: firstTime
      !   integer,intent(in) :: n
      !   integer :: un
      !   if (firstTime) then
      !     un = newAndOpen(dir,name)
      !     call writeTecPlotHeader(un,name)
      !   else
      !     un = openToAppend(dir,name)
      !   endif
      !   write(un,'(5'//arrfmt//')') x,y,z,real(n,cp),val
      !   close(un)
      ! end subroutine

      subroutine writeTransientToFileRes(n,val,dir,name,un)
        character(len=*),intent(in) :: dir,name
        real(cp),intent(in) :: val
        integer,intent(in) :: n,un
        write(un,'(2'//arrfmt//')') real(n,cp),val
      end subroutine

      end module