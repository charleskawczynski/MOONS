      module IO_scalarBase_mod
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
      public :: writeToFile
      public :: readFromFile

      logical,parameter :: headerTecplot = .true.

      interface readFromFile;  module procedure read3DFieldFromFile;    end interface
      interface writeToFile;   module procedure write3DFieldToFile;     end interface

      ! WRITE ONLY

      interface writeToFile;   module procedure write3DMeshToFile;      end interface

      interface writeToFile;   module procedure write0DFieldsToFile;    end interface
      interface writeToFile;   module procedure write0DFieldToFile;     end interface
      interface writeToFile;   module procedure write1DFieldToFile;     end interface
      interface writeToFile;   module procedure write2DFieldToFile;     end interface

      contains

      subroutine read3DFieldFromFile(x,y,z,arr,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        logical,intent(in),optional :: headerTecplotTemp
        real(cp),dimension(:),intent(inout) :: x,y,z
        real(cp),dimension(:,:,:),intent(inout) :: arr
        integer un,i,j,k,sx,sy,sz

        sx = size(x); sy = size(y); sz = size(z)
        un = openToRead(dir,name)

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) then
            read(un,*);read(un,*);read(un,*)
          endif
        else
          if (headerTecplot) then
            read(un,*);read(un,*);read(un,*)
          endif
        endif

        do k = 1,sz
          do j = 1,sy
            do i = 1,sx
              read(un,'(4'//rarrfmt//')') x(i),y(j),z(k),arr(i,j,k)
            enddo
          enddo
        enddo
        call closeExisting(un,name,dir)
      end subroutine

      subroutine write3DFieldToFile(x,y,z,arr,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        real(cp),dimension(:),intent(in) :: x,y,z
        real(cp),dimension(:,:,:),intent(in) :: arr
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,j,k,sx,sy,sz
        integer,dimension(3) :: s
        s = shape(arr)
        sx = size(x); sy = size(y); sz = size(z)
        u = newAndOpen(dir,name)
        
        if (s(1).ne.sx.or.s(2).ne.sy.or.s(3).ne.sz) then
          write(*,*) 'Error: IO Mismatch of sizes in ' // trim(adjustl(name)); stop
        endif

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name,sx,sy,sz)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name,sx,sy,sz)
        endif

        do k = 1,sz
          do j = 1,sy
            do i = 1,sx
              write(u,'(4'//arrfmt//')') x(i),y(j),z(k),arr(i,j,k)
            enddo
          enddo
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write3DMeshToFile(x,y,z,val,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        real(cp),dimension(:),intent(in) :: x,y,z
        real(cp),intent(in) :: val
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,j,k,sx,sy,sz
        sx = size(x); sy = size(y); sz = size(z)
        u = newAndOpen(dir,name)

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name,sx,sy,sz)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name,sx,sy,sz)
        endif

        do k = 1,sz
          do j = 1,sy
            do i = 1,sx
              write(u,'(4'//arrfmt//')') x(i),y(j),z(k),val
            enddo
          enddo
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write0DFieldsToFile(arr1,arr2,arr3,arr4,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        real(cp),dimension(:),intent(in) :: arr1,arr2,arr3,arr4
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,s
        s = size(arr1)
        u = newAndOpen(dir,name)

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name)
        endif

        do i = 1,s
          write(u,'(4'//arrfmt//')') arr1(i),arr2(i),arr3(i),arr4(i)
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write0DFieldToFile(arr,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        real(cp),dimension(:),intent(in) :: arr
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,s
        s = size(arr)
        u = newAndOpen(dir,name)

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name)
        endif

        do i = 1,s
          write(u,'('//arrfmt//')') arr(i)
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write1DFieldToFile(x,arr,dir,name,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name
        real(cp),dimension(:),intent(in) :: x,arr
        logical,intent(in),optional :: headerTecplotTemp
        integer :: u,i,sx,s
        sx = size(x)
        u = newAndOpen(dir,name)
        s = size(arr)

        if (s.ne.sx) then
          write(*,*) 'Error: IO Mismatch of sizes in ' // trim(adjustl(name)); stop
        endif

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name,sx)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name,sx)
        endif

        do i = 1,sx
          write(u,'(2'//arrfmt//')') x(i),arr(i)
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write2DFieldToFile(x,y,arr,dir,name,ext,n,headerTecplotTemp)
        implicit none
        character(len=*),intent(in) :: dir,name,ext
        real(cp),dimension(:),intent(in) :: x,y
        real(cp),dimension(:,:),intent(in) :: arr
        integer,intent(in) :: n
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,j,sx,sy
        sx = size(x); sy = size(y)
        u = newAndOpen(dir,trim(adjustl(name))//trim(adjustl(ext)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeaderTransient(u,name,sx,sy,n)
        else
          if (headerTecplot) call writeTecPlotHeaderTransient(u,name,sx,sy,n)
        endif

        do j = 1,sy
          do i = 1,sx
            write(u,'(3'//arrfmt//')') x(i),y(j),arr(i,j)
          enddo
        enddo
        call closeAndMessage(u,trim(adjustl(name))//trim(adjustl(ext)),dir)
      end subroutine

      end module