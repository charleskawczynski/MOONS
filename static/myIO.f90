      module myIO_mod
      use constants_mod
      implicit none

     ! Fixes / Improvements:
     ! Make a buildDirectory routine:
     ! http://homepages.wmich.edu/~korista/README-fortran.html

      private
      
      public :: writeKillSwitchToFile
      public :: readKillSwitchFromFile
      public :: readLastStepFromFile
      public :: writeLastStepToFile

      public :: writeToFile
      public :: readFromFile
      public :: writeTransientToFile
      public :: makeDir,rmDir
      public :: arrfmt
      public :: newUnit
      public :: getUnit
      public :: newAndOpen
      public :: closeAndMessage
      public :: int2Str,num2Str

      character(len=4),parameter :: fileType = '.dat'
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

      interface readFromFile
        module procedure read3DFieldFromFile
        module procedure read3DVecFieldFromFile
      end interface

      interface writeTransientToFile
        module procedure writeTransientToFileXYZ
        module procedure writeTransientToFileRes
      end interface

      interface writeToFile
        module procedure write0DFieldsToFile
        module procedure write0DFieldToFile
        module procedure write1DFieldToFile

        module procedure write2DFieldToFile
        module procedure write3DFieldToFile

        module procedure write2DVecFieldToFile
        module procedure write3DVecFieldToFile
        module procedure write3DMeshToFile
      end interface

      interface writeTecPlotHeader
        module procedure writeTecPlotHeader0DField
        module procedure writeTecPlotHeader1DField

        module procedure writeTecPlotHeader2DScalarField
        module procedure writeTecPlotHeader3DScalarField

        module procedure writeTecPlotHeader2DVectorField
        module procedure writeTecPlotHeader3DVectorField
      end interface

      contains

      subroutine makeDir(d1,d2,d3)
        character(len=*),intent(in) :: d1
        character(len=*),intent(in),optional :: d2
        character(len=*),intent(in),optional :: d3
        character(len=30) :: d
        logical :: ex
        if (present(d2).and.present(d3)) then
          d = adjustl(trim(d1)) // adjustl(trim(d2)) // adjustl(trim(d3))
        elseif (present(d2)) then
          d = adjustl(trim(d1)) // adjustl(trim(d2))
        else
          d = adjustl(trim(d1))
        endif

        INQUIRE (file=adjustl(trim(d)), EXIST=ex)
        if (.not.ex) then
          call system('mkdir ' // adjustl(trim(d)) )
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' created.'
        else 
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' already exists.'
        endif
      end subroutine

      subroutine rmDir(d)
        character(len=*),intent(in) :: d
          call system('rm /' // adjustl(trim(d)) )
          write(*,*) 'Directory ' // adjustl(trim(d)) // ' removed.'
      end subroutine

      function newAndOpen(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,n
        NU = newUnit()
        open(NU,file=trim(strcompress(dir,n)) // trim(strcompress(name,n)) // fileType,pad='YES')
      end function

      function getUnit(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU
        NU = openToRead(dir,name)
      end function

      function openToAppend(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        NU = newUnit()
        inquire(file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType, &
        number=NU,exist=ex,opened=op,action=act)

        if (.not.op) then
          NU = newUnit()
        endif
        if (ex) then
          open(NU,file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType,&
          status = 'old', action = 'readwrite',iostat=ok,position='append')
        else
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' does not exist. Terminating execution.';call myPause()
        endif
        if (ok.ne.0) then
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' was not opened successfully.';call myPause()
        endif
      end function

      function openToRead(dir,name) result(NU)
        implicit none
        character(len=*),intent(in) :: dir,name
        integer :: NU,ok
        logical :: ex,op
        character(len=9) :: act
        NU = newUnit()
        inquire(file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType, &
        number=NU,exist=ex,opened=op,action=act)

        if (.not.op) then
          NU = newUnit()
        endif
        if (ex) then
          open(NU,file=trim(adjustl(dir)) // trim(adjustl(name)) // fileType,&
          status = 'old', action = 'read',iostat=ok)
        else
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' does not exist. Terminating execution.';call myPause()
        endif
        if (ok.ne.0) then
          write(*,*) 'The file ' // trim(adjustl(dir)) // trim(adjustl(name)) &
          // fileType // ' was not opened successfully.';call myPause()
        endif
      end function

      subroutine closeAndMessage(u,name,dir)
        ! BONEHEAD MOVE: should be dir,name,u
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: name,dir
        close(u)
        write(*,*) '+++ Data for ' // trim(adjustl(name)) // ' written to ' // trim(adjustl(dir)) //' +++'
      end subroutine

      subroutine closeExisting(u,name,dir)
        implicit none
        integer,intent(in) :: u
        character(len=*),intent(in) :: name,dir
        close(u)
        write(*,*) '+++ Data for ' // name // ' read from ' // dir //' +++'
      end subroutine

      function newUnit() result(nu)
        implicit none
        ! local
        integer, parameter :: LUN_MIN=10, LUN_MAX=1000
        logical :: opened
        integer :: lun,nu
        ! begin
        nu=-1
        do lun=LUN_MIN,LUN_MAX
          inquire(unit=lun,opened=opened)
          if (.not. opened) then
            nu=lun
          exit
          endif
        enddo
      end function

      function int2Str(i) result(s)
        implicit none
        integer,intent(in) :: i
        character(len=5) :: s
        write(s,'(I5)') i
        s = trim(adjustl(s))
      end function

      function num2Str(i) result(s)
        implicit none
        real(dpn),intent(in) :: i
        character(len=8) :: s
        write(s,'(F3.5)') i
        s = trim(adjustl(s))
      end function

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

      subroutine writeKillSwitchToFile(ks,dir,name)
        character(len=*),intent(in) :: dir,name
        logical,intent(in) :: ks
        integer :: un
        un = newAndOpen(dir,name)
        write(un,'('//logfmt//')') ks
        call closeAndMessage(un,name,dir)
      end subroutine

      subroutine readKillSwitchFromFile(ks,dir,name)
        character(len=*),intent(in) :: dir,name
        logical,intent(inout) :: ks
        integer :: un
        un = openToRead(dir,name)
        read(un,'('//logfmt//')') ks
        call closeExisting(un,name,dir)
      end subroutine

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

      subroutine read3DVecFieldFromFile(x,y,z,u,v,w,dir,namex,namey,namez,headerTecplotTemp)
        character(len=*),intent(in) :: dir,namex,namey,namez
        real(dpn),dimension(:),intent(inout) :: x,y,z
        real(dpn),dimension(:,:,:),intent(inout) :: u,v,w
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,k,sx,sy,sz
        integer,dimension(3) :: s
        s = shape(u)
        sx = size(x); sy = size(y); sz = size(z)
        if (s(1).ne.sx.or.s(2).ne.sy.or.s(3).ne.sz) then
          write(*,*) 'Mismatch of sizes in ' // trim(adjustl(namex)); stop
        endif

        un = openToRead(dir,trim(adjustl(namex))//','&
        //trim(adjustl(namey))//','//trim(adjustl(namez)))

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
               read(un,'(6'//rarrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
             enddo
           enddo
         enddo

        call closeExisting(un,trim(adjustl(namex))//','&
        //trim(adjustl(namey))//','//trim(adjustl(namez)),dir)
      end subroutine

      subroutine writeTransientToFileXYZ(x,y,z,n,val,dir,name,firstTime)
        ! Fixes:
        ! Output location then start iterating with time step and 
        ! value, do not repeat location at every time step...
        character(len=*),intent(in) :: dir,name
        real(dpn),intent(in) :: x,y,z,val
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
        real(dpn),intent(in) :: val
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

      subroutine write0DFieldsToFile(arr1,arr2,arr3,arr4,dir,name,headerTecplotTemp)
        character(len=*),intent(in) :: dir,name
        real(dpn),dimension(:),intent(in) :: arr1,arr2,arr3,arr4
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

      subroutine write3DVecFieldToFile(x,y,z,u,v,w,dir,namex,namey,namez,headerTecplotTemp)
        character(len=*),intent(in) :: dir,namex,namey,namez
        real(dpn),dimension(:),intent(in) :: x,y,z
        real(dpn),dimension(:,:,:),intent(in) :: u,v,w
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,k,sx,sy,sz
        sx = size(x); sy = size(y); sz = size(z)
        un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(un,namex,namey,namez,sx,sy,sz)
        else
          if (headerTecplot) call writeTecPlotHeader(un,namex,namey,namez,sx,sy,sz)
        endif

         do k = 1,sz
           do j = 1,sy
             do i = 1,sx
               write(un,'(6'//arrfmt//')') x(i),y(j),z(k),u(i,j,k),v(i,j,k),w(i,j,k)
             enddo
           enddo
         enddo
        call closeAndMessage(un,trim(adjustl(namex))//','//trim(adjustl(namey))//','//trim(adjustl(namez)),dir)
      end subroutine

      subroutine write2DVecFieldToFile(x,y,u,v,dir,namex,namey,headerTecplotTemp)
        character(len=*),intent(in) :: dir,namex,namey
        real(dpn),dimension(:),intent(in) :: x,y
        real(dpn),dimension(:,:),intent(in) :: u,v
        logical,intent(in),optional :: headerTecplotTemp
        integer un,i,j,sx,sy
        sx = size(x); sy = size(y)
        un = newAndOpen(dir,trim(adjustl(namex))//','//trim(adjustl(namey)))

        if (present(headerTecplotTemp)) then
          if (headerTecplotTemp) call writeTecPlotHeader(un,namex,namey,sx,sy)
        else
          if (headerTecplot) call writeTecPlotHeader(un,namex,namey,sx,sy)
        endif

         do j = 1,sy
           do i = 1,sx
             write(un,'(4'//arrfmt//')') x(i),y(j),u(i,j),v(i,j)
           enddo
         enddo
        call closeAndMessage(un,trim(adjustl(namex))//','//trim(adjustl(namey)),dir)
      end subroutine

      !*********************************************************************
      !********************** TEC PLOT HEADERS *****************************
      !*********************************************************************
      !------------------------ SCALAR FIELDS ------------------------------

      subroutine writeTecPlotHeader3DScalarField(u,name,sx,sy,sz)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: sx,sy,sz,u
        integer :: sn
        sn = len(name)

        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "3D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+26)//')') 'VARIABLES = "X","Y","Z",'//'"'//trim(adjustl(name))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sz))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ', K = ',trim(adjustl(int2str(sz))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine writeTecPlotHeader2DScalarField(u,name,sx,sy)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: sx,sy,u
        integer :: sn
        sn = len(name)

        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//trim(adjustl(name))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !--------------------------------------- VECTOR FIELDS ---------------------------------------------

      subroutine writeTecPlotHeader3DVectorField(u,namex,namey,namez,sx,sy,sz)
        implicit none
        character(len=*),intent(in) :: namex,namey,namez
        integer,intent(in) :: sx,sy,sz,u
        integer :: snx,sny,snz
        snx = len(namex); sny = len(namey); snz = len(namez)

        write(u,'(A'//int2Str(snx+sny+snz+28)//')') 'TITLE = "3D Vector Field '  & 
        // trim(adjustl(namex)) //','// trim(adjustl(namey)) //','// trim(adjustl(namez)) // '"'
        write(u,'(A'//int2Str(snx+sny+snz+32)//')') 'VARIABLES = "X","Y","Z",' & 
        //'"'//trim(adjustl(namex))//'",'//'"'//trim(adjustl(namey))//'",'//'"'//trim(adjustl(namez))//'"'

        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sz))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ', K = ',trim(adjustl(int2str(sz))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine writeTecPlotHeader2DVectorField(u,namex,namey,sx,sy)
        implicit none
        character(len=*),intent(in) :: namex,namey
        integer,intent(in) :: sx,sy,u
        integer :: snx,sny
        snx = len(namex); sny = len(namey)

        write(u,'(A'//int2Str(snx+sny+27)//')') 'TITLE = "2D Vector Field '  & 
        // trim(adjustl(namex)) //','// trim(adjustl(namey)) // '"'
        write(u,'(A'//int2Str(snx+sny+25)//')') 'VARIABLES = "X","Y",' & 
        //'"'//trim(adjustl(namex))//'",'//'"'//trim(adjustl(namey))//'"'

        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !----------------------------------------  1D/0D FIELDS --------------------------------------------

      subroutine writeTecPlotHeader1DField(u,namex,sx)
        implicit none
        character(len=*),intent(in) :: namex
        integer,intent(in) :: sx,u
        integer :: snx
        snx = len(namex)

        write(u,'(A'//int2Str(snx+26)//')') 'TITLE = "1D Field '  & 
        // trim(adjustl(namex)) // '"'
        write(u,'(A'//int2Str(snx+18)//')') 'VARIABLES = "X",' & 
        //'"'//trim(adjustl(namex))//'"'

        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine writeTecPlotHeader0DField(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(name)
        write(u,'(A'//int2Str(sn+19)//')') 'TITLE = "0D Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = '//'"'//trim(adjustl(name)) //'"'
        write(u,'(A24)') 'ZONE DATAPACKING = POINT'
      end subroutine

      !***************************************************************************************************
      !***************************************************************************************************
      !***************************************************************************************************

      FUNCTION strcompress( input_string, n ) RESULT ( output_string ) 
        !------------------------------------------------------------------------------ 
        ! NAME: 
        !       strcompress 
        ! 
        ! PURPOSE: 
        !       Function to return a copy of an input string with all whitespace 
        !       (spaces and tabs) removed. 
        ! 
        ! CATEGORY: 
        !       Utility 
        ! 
        ! LANGUAGE: 
        !       Fortran-90 
        ! 
        ! CALLING SEQUENCE: 
        !       output_string = strcompress( input_string,  &  ! Input 
        !                                    n              )  ! Output 
        ! 
        ! INPUT ARGUMENTS: 
        !       input_string:  Character string to be compressed. 
        !                      UNITS:      None 
        !                      TYPE:       Character 
        !                      DIMENSION:  Scalar, LEN = * 
        !                      ATTRIBUTES: INTENT( IN ) 
        ! 
        ! OPTIONAL INPUT ARGUMENTS: 
        !       None. 
        ! 
        ! OUTPUT ARGUMENTS: 
        !       n:             Number of useful characters in output string 
        !                      after compression. From character n+1 -> LEN( input_string ) 
        !                      the output is padded with blanks. 
        !                      UNITS:      None 
        !                      TYPE:       Integer 
        !                      DIMENSION:  Scalar 
        !                      ATTRIBUTES: INTENT( OUT ) 
        ! 
        ! OPTIONAL OUTPUT ARGUMENTS: 
        !       None. 
        ! 
        ! FUNCTION RESULT: 
        !       Input character string with internal white space (spaces and tabs) 
        !       removed. Returned string length is still the length of the input 
        !       string, but compressed and padded with blanks. 
        ! 
        ! CALLS: 
        !       None. 
        ! 
        ! SIDE EFFECTS: 
        !       None. 
        ! 
        ! EXAMPLE: 
        !       input_string = '  This is a string with spaces in it.' 
        !       output_string = strcompress( input_string, n ) 
        !       WRITE( *, '( a )' ) '>',output_string( 1:n ),'<' 
        !   >Thisisastringwithspacesinit.< 
        ! 
        !       or 
        ! 
        !       WRITE( *, '( a )' ) '>',TRIM( output_string ),'<' 
        !   >Thisisastringwithspacesinit.< 
        ! 
        ! PROCEDURE: 
        !       Definitions of a space and a tab character are made for the 
        !       ASCII collating sequence. Each single character of the input 
        !       string is checked against these definitions using the IACHAR() 
        !       intrinsic. If the input string character DOES NOT correspond 
        !       to a space or tab, it is not copied to the output string. 
        ! 
        !       Note that for input that ONLY has spaces or tabs BEFORE the first 
        !       useful character, the output of this function is the same as the 
        !       ADJUSTL() instrinsic. 
        ! 
        ! CREATION HISTORY: 
        !       Written by:     Paul van Delst, CIMSS/SSEC 18-Oct-1999 
        !------------------------------------------------------------------------------ 
        CHARACTER( * ), INTENT( IN )  :: input_string 
        INTEGER,        INTENT( OUT ) :: n 
        ! -- Function result 
        CHARACTER( LEN( input_string ) ) :: output_string 
        ! -- Local parameters 
        INTEGER,        PARAMETER :: IACHAR_SPACE = 32, & 
                                     IACHAR_TAB   = 9 
        ! -- Local variables 
        INTEGER :: i 
        INTEGER :: iachar_character 
        ! -- Initialise output string 
        output_string = ' ' 
        ! -- Initialise output string "useful" length counter 
        n = 0 
        ! -- Loop over string elements 
        DO i = 1, LEN( input_string ) 
          ! -- Convert the current character to its position 
          ! -- in the ASCII collating sequence 
          iachar_character = IACHAR( input_string( i:i ) ) 
          ! -- If the character is NOT a space ' ' or a tab '->|' 
          ! -- copy it to the output string. 
          IF ( iachar_character /= IACHAR_SPACE .AND. & 
               iachar_character /= IACHAR_TAB         ) THEN 
            n = n + 1 
            output_string( n:n ) = input_string( i:i ) 
          END IF 
        END DO 
      END FUNCTION strcompress

      end module