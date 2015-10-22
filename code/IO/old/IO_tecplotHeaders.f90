      module IO_tecplotHeaders_mod
      use IO_tools_mod
      implicit none

      private
      
      public :: writeTecPlotHeader
      public :: writeTecPlotHeaderTransient

      interface writeTecPlotHeader; module procedure writeTPH0DField;        end interface
      interface writeTecPlotHeader; module procedure writeTPH1DField;        end interface
      interface writeTecPlotHeader; module procedure writeTPH2DScalarField;  end interface
      interface writeTecPlotHeader; module procedure writeTPH3DScalarField;  end interface
      interface writeTecPlotHeader; module procedure writeTPH2DVectorField;  end interface
      interface writeTecPlotHeader; module procedure writeTPH3DVectorField;  end interface

      interface writeTecPlotHeaderTransient; module procedure writeTPH2DVectorFieldTransient; end interface
      interface writeTecPlotHeaderTransient; module procedure writeTPH2DScalarFieldTransient; end interface

      contains

      !*********************************************************************
      !************************ SCALAR FIELDS ******************************
      !*********************************************************************

      subroutine writeTPH3DScalarField(u,name,s)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in),dimension(3) :: s
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))

        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "3D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+26)//')') 'VARIABLES = "X","Y","Z",'//'"'//trim(adjustl(name))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(s(1)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(2)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(3)))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',trim(adjustl(int2str(s(1)))), &
                ', J = ',trim(adjustl(int2str(s(2)))), &
                ', K = ',trim(adjustl(int2str(s(3)))), &
                ' DATAPACKING = POINT'
      end subroutine

!       subroutine writeTPH2DScalarField(u,name,sx,sy)
!         implicit none
!         character(len=*),intent(in) :: name
!         integer,intent(in) :: sx,sy,u
!         integer :: sn
!         sn = len(name)

!         write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field '//trim(adjustl(name))//'"'
!         write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//trim(adjustl(name))//'"'
!         write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(sx))))) // & 
!                 ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
!                 ',A20)') 'ZONE ', &
!                 ', I = ',trim(adjustl(int2str(sx))), &
!                 ', J = ',trim(adjustl(int2str(sy))), &
!                 ' DATAPACKING = POINT'
!       end subroutine

      subroutine writeTPH2DScalarField(u,name,sx,sy)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: sx,sy,u
        character(len=:),allocatable :: temp
        integer :: sn
        sn = len(name)
        allocate(character(len=sn) :: temp)
        temp = strcompress(name,sn)

        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field '//temp //'"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//temp//'"'
        write(u,'(A5,A6,A' // int2str(intLen(sx)) // & 
                ',A6,A'    // int2str(intLen(sy)) // & 
                ',A20)') 'ZONE ', &
                ', I = ',trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
        deallocate(temp)
      end subroutine


      subroutine writeTPH2DScalarFieldTransient(u,name,sx,sy,t)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: sx,sy,u,t
        integer :: sn
        sn = len(name)
        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field '//trim(adjustl(name))// '"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//trim(adjustl(name))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t)))))  // & 
                ',A,A6,A'  // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',trim(adjustl(int2str(t))),'"', &
                ', I = ',trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !*********************************************************************
      !************************ VECTOR FIELDS ******************************
      !*********************************************************************

      subroutine writeTPH3DVectorField(u,namex,namey,namez,s)
        implicit none
        character(len=*),intent(in) :: namex,namey,namez
        integer,intent(in),dimension(3) :: s
        integer,intent(in) :: u
        integer :: snx,sny,snz
        snx = len(namex); sny = len(namey); snz = len(namez)
        write(u,'(A'//int2Str(snx+sny+snz+28)//')') 'TITLE = "3D Vector Field '  & 
        // trim(adjustl(namex)) //','// trim(adjustl(namey)) //','// trim(adjustl(namez)) // '"'
        write(u,'(A'//int2Str(snx+sny+snz+32)//')') 'VARIABLES = "X","Y","Z",' & 
        //'"'//trim(adjustl(namex))//'",'//'"'//trim(adjustl(namey))//'",'//'"'//trim(adjustl(namez))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(s(1)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(2)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(3)))))) // & 
                ',A20)') 'ZONE ', &
                ', I = ',trim(adjustl(int2str(s(1)))), &
                ', J = ',trim(adjustl(int2str(s(2)))), &
                ', K = ',trim(adjustl(int2str(s(3)))), &
                ' DATAPACKING = POINT'
      end subroutine

      ! subroutine writeTPH3DVectorField(u,namex,namey,namez,s)
      !   implicit none
      !   character(len=*),intent(in) :: namex,namey,namez
      !   integer,intent(in) :: s,u
      !   integer :: snx,sny,snz,sn
      !   snx = len(namex); sny = len(namey); snz = len(namez)
      !   sn = snx+sny+snz
      !   allocate(character(len=snx) :: tempx)
      !   allocate(character(len=sny) :: tempy)
      !   allocate(character(len=snz) :: tempz)
      !   tempx = strcompress(namex,snx)
      !   tempy = strcompress(namey,sny)
      !   tempz = strcompress(namez,snz)
      !   write(u,'(A'//int2Str(sn+28)//')') 'TITLE = "3D Vector Field '  & 
      !   //tempx//','//tempy//','//tempz//'"'
      !   write(u,'(A'//int2Str(sn+32)//')') 'VARIABLES = "X","Y","Z",' & 
      !   //'"'//tempx//'",'//'"'//tempy//'",'//'"'//tempz//'"'
      !   write(u,'(A5,A6,A' // int2str(intLen(sx)) // & 
      !           ',A6,A'    // int2str(intLen(sy)) // & 
      !           ',A6,A'    // int2str(intLen(sz)) // & 
      !           ',A20)') 'ZONE ', &
      !           ', I = ',trim(adjustl(int2str(sx))), &
      !           ', J = ',trim(adjustl(int2str(sy))), &
      !           ', K = ',trim(adjustl(int2str(sz))), &
      !           ' DATAPACKING = POINT'
      !   deallocate(tempx,tempy,tempz)
      ! end subroutine

      subroutine writeTPH2DVectorField(u,namex,namey,sx,sy)
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
                ', I = ',trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine writeTPH2DVectorFieldTransient(u,namex,namey,sx,sy,t)
        implicit none
        character(len=*),intent(in) :: namex,namey
        integer,intent(in) :: sx,sy,u,t
        integer :: snx,sny
        snx = len(namex); sny = len(namey);

        write(u,'(A'//int2Str(snx+sny+27)//')') 'TITLE = "2D Vector Field '  & 
        // trim(adjustl(namex)) //','// trim(adjustl(namey)) // '"'
        write(u,'(A'//int2Str(snx+sny+25)//')') 'VARIABLES = "X","Y",' & 
        //'"'//trim(adjustl(namex))//'",'//'"'//trim(adjustl(namey))//'"'

        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t))))) // & 
                ',A,A6,A'  // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',trim(adjustl(int2str(t))),'"', &
                ', I = ',trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !*********************************************************************
      !************************* 1D/0D FIELDS ******************************
      !*********************************************************************

      subroutine writeTPH1DField(u,namex,sx)
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
                ', I = ',trim(adjustl(int2str(sx))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine writeTPH0DField(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(name)
        write(u,'(A'//int2Str(sn+19)//')') 'TITLE = "0D Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+17)//')') 'VARIABLES = N ,'//'"'//trim(adjustl(name)) //'"'
        write(u,'(A24)') 'ZONE DATAPACKING = POINT'
      end subroutine


      end module