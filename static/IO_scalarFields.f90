      module IO_scalarFields_mod
      use constants_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

      private

      public :: writeToFile
      public :: readFromFile
      public :: writeTransientToFile

       ! This website is a good reference for formatting:
       ! http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap05/format.html
       ! leading digit + . + precision + E + exponent + signs + spaces between
       !       1         1      12       1      3         2          3
       ! rarrfmt is for reading (possible old formats)
       !  arrfmt is for writing (current format)
      character(len=8),parameter :: rarrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=8),parameter ::  arrfmt = 'E23.12E3'  ! Make sure length is correct when adjusting
      character(len=3),parameter ::  intfmt = 'I10'       ! Make sure length is correct when adjusting
      character(len=3),parameter ::  logfmt = 'L1'        ! Make sure length is correct when adjusting

      logical,parameter :: headerTecplot = .true.

      interface readFromFile;  module procedure read3DFieldFromFile;   end interface

      interface writeToFile;   module procedure write0DFieldToFile;    end interface
      interface writeToFile;   module procedure write1DFieldToFile;    end interface
      interface writeToFile;   module procedure write2DFieldToFile;    end interface
      interface writeToFile;   module procedure write3DFieldToFile;    end interface

      interface writeToFile;   module procedure write3DMeshToFile;     end interface
        
      contains

      subroutine read3DFieldFromFile(x,y,z,arr,dir,name,headerTecplotTemp)
        character(len=*),intent(in) :: dir,name
        logical,intent(in),optional :: headerTecplotTemp
        real(dpn),dimension(:) :: x,y,z
        real(dpn),dimension(:,:,:),intent(inout) :: arr
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

      subroutine write0DFieldToFile(arr,dir,name,headerTecplotTemp)
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: arr
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
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: x,arr
        logical,intent(in),optional :: headerTecplotTemp
        integer :: u,i,sx
        sx = size(x)
        u = newAndOpen(dir,name)

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

      subroutine write2DFieldToFile(x,y,arr,dir,name,headerTecplotTemp)
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: x,y
        real(dpn),dimension(:,:),intent(in) :: arr
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,j,sx,sy
        sx = size(x); sy = size(y)
        u = newAndOpen(dir,name)

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(u,name,sx,sy)
        else
          if (headerTecplot) call writeTecPlotHeader(u,name,sx,sy)
        endif

        do j = 1,sy
          do i = 1,sx
            write(u,'(3'//arrfmt//')') x(i),y(j),arr(i,j)
          enddo
        enddo
        call closeAndMessage(u,name,dir)
      end subroutine

      subroutine write3DFieldToFile(x,y,z,arr,dir,name,headerTecplotTemp)
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: x,y,z
        real(dpn),dimension(:,:,:),intent(in) :: arr
        logical,intent(in),optional :: headerTecplotTemp
        integer u,i,j,k,sx,sy,sz
        integer,dimension(3) :: s
        s = shape(arr)
        sx = size(x); sy = size(y); sz = size(z)
        u = newAndOpen(dir,name)
        
        if (s(1).ne.sx.or.s(2).ne.sy.or.s(3).ne.sz) then
          write(*,*) 'Mismatch of sizes in ' // trim(adjustl(name)); stop
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
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: x,y,z
        real(dpn),intent(in) :: val
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



      end module