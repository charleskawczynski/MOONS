      module exp_Tecplot_Header_mod
      use current_precision_mod
      use IO_tools_mod
      implicit none

      private
      
      public :: exp_Header_3D_1C,exp_Header_3D_3C ! 3D
      public :: exp_Header_2D_1C,exp_Header_2D_2C ! 2D
      public :: exp_Header_2D_3C ! 2D/3D
      public :: exp_Header_1D_1C,exp_Header_0D_1C ! 1D/0D

      contains

      ! *****************************************************
      ! ********************* 3D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_3D_1C(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "3D Scalar Field"'
        write(u,'(A'//int2Str(sn+26)//')') 'VARIABLES = "x","y","z",'&
        //'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Header_3D_3C(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "3D Vector Field"'
        write(u,'(A'//int2Str(3*sn+38)//')') 'VARIABLES = "x","y","z",'& 
        //'"'//trim(adjustl(name))//'_x",'&
        //'"'//trim(adjustl(name))//'_y",'&
        //'"'//trim(adjustl(name))//'_z"'
      end subroutine

      ! *****************************************************
      ! ********************* 2D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_2D_1C(u,name,dir)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Scalar Field"'
        select case (dir)
        case (1); write(u,'(A'//int2Str(sn+24)//')') 'VARIABLES = "y","z",'& 
                  //'"'//trim(adjustl(name))//'_x"'
        case (2); write(u,'(A'//int2Str(sn+24)//')') 'VARIABLES = "x","z",'& 
                  //'"'//trim(adjustl(name))//'_y"'
        case (3); write(u,'(A'//int2Str(sn+24)//')') 'VARIABLES = "x","y",'& 
                  //'"'//trim(adjustl(name))//'_z"'
        case default; stop 'Error: dir must = 1,2,3 in exp_Header_1C_2D in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_2D_2C(u,name,dir)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Vector Field"'
        select case (dir)
        case (1); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "y","z",'& 
                  //'"'//trim(adjustl(name))//'_y",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (2); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "x","z",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (3); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "x","y",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_y"'
        case default; stop 'Error: dir must = 1,2,3 in exp_Header_2C_2D in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_2D_3C(u,name,dir)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Vector Field"'
        select case (dir)
        case (1); write(u,'(A'//int2Str(3*sn+34)//')') 'VARIABLES = "y","z",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_y",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (2); write(u,'(A'//int2Str(3*sn+34)//')') 'VARIABLES = "x","z",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_y",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (3); write(u,'(A'//int2Str(3*sn+34)//')') 'VARIABLES = "x","y",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_y",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case default; stop 'Error: dir must = 1,2,3 in exp_Header_2C_2D in exp_Tec_Header.f90'
        end select
      end subroutine

      ! *****************************************************
      ! ********************* 1D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_1D_1C(u,name,dir)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "1D Scalar Field"'
        select case (dir)
        case (1); write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "x",'//'"'//trim(adjustl(name))//'"'
        case (2); write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "y",'//'"'//trim(adjustl(name))//'"'
        case (3); write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "z",'//'"'//trim(adjustl(name))//'"'
        case default; stop 'Error: dir must = 1,2,3 in exp_Header_1C_1D in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_0D_1C(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "0D Scalar Field"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "N",'//'"'//trim(adjustl(name))//'"'
      end subroutine

      end module