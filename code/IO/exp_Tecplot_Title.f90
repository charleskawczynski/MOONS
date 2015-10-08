      module exp_Tec_Title_mod
      use IO_tools_mod
      implicit none

      private
      
      public :: exp_Title_3D,exp_Title_2D
      public :: exp_Title_1D,exp_Title_0D

      contains

      subroutine exp_Title_3D(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "3D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+26)//')') 'VARIABLES = "X","Y","Z",'//'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Title_2D(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "2D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'//'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Title_1D(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "1D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "X",'//'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Title_0D(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A'//int2Str(sn+26)//')') 'TITLE = "0D Scalar Field ' // trim(adjustl(name)) // '"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "N",'//'"'//trim(adjustl(name))//'"'
      end subroutine

      end module