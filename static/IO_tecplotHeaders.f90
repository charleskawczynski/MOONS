      module IO_tecplotHeaders_mod
      use IO_tools_mod
      implicit none

      private
      
      public :: writeTecPlotHeader
      public :: writeTecPlotHeaderTransient

      interface writeTecPlotHeader
        module procedure writeTecPlotHeader0DField
        module procedure writeTecPlotHeader1DField

        module procedure writeTecPlotHeader2DScalarField
        module procedure writeTecPlotHeader3DScalarField

        module procedure writeTecPlotHeader2DVectorField
        module procedure writeTecPlotHeader3DVectorField
      end interface

      interface writeTecPlotHeaderTransient
        module procedure writeTecPlotHeader2DVectorFieldTransient
        module procedure writeTecPlotHeader2DScalarFieldTransient
      end interface

      contains

      !*********************************************************************
      !************************ SCALAR FIELDS ******************************
      !*********************************************************************

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

      subroutine writeTecPlotHeader2DScalarFieldTransient(u,name,sx,sy,t)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: sx,sy,u,t
        integer :: sn
        sn = len(name)

        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//trim(adjustl(name))//'"'
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t))))) // & 
                ',A,A6,A'    // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',  trim(adjustl(int2str(t))),'"', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !*********************************************************************
      !************************ VECTOR FIELDS ******************************
      !*********************************************************************

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

      subroutine writeTecPlotHeader2DVectorFieldTransient(u,namex,namey,sx,sy,t)
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
                ',A,A6,A'    // int2str(len(trim(adjustl(int2str(sx))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(sy))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',  trim(adjustl(int2str(t))),'"', &
                ', I = ',  trim(adjustl(int2str(sx))), &
                ', J = ',trim(adjustl(int2str(sy))), &
                ' DATAPACKING = POINT'
      end subroutine

      !*********************************************************************
      !************************* 1D/0D FIELDS ******************************
      !*********************************************************************

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
        write(u,'(A'//int2Str(sn+17)//')') 'VARIABLES = N ,'//'"'//trim(adjustl(name)) //'"'
        write(u,'(A24)') 'ZONE DATAPACKING = POINT'
      end subroutine


      end module