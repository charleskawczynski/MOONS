      module exp_Tecplot_Header_mod
      use IO_tools_mod
      implicit none

      private
      
      public :: exp_Header_3D_SF
      public :: exp_Header_3D_VF

      public :: exp_Header_2D_SF
      public :: exp_Header_2D_VF
      public :: exp_Header_2D_VF_dir

      public :: exp_Header_1D_SF
      public :: exp_Header_0D_SF

      contains

      ! *****************************************************
      ! ********************* 3D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_3D_SF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "3D Scalar Field"'
        write(u,'(A'//int2Str(sn+26)//')') 'VARIABLES = "X","Y","Z",'&
        //'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Header_3D_VF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "3D Vector Field"'
        write(u,'(A'//int2Str(3*sn+38)//')') 'VARIABLES = "X","Y","Z",'& 
        //'"'//trim(adjustl(name))//'_x",'&
        //'"'//trim(adjustl(name))//'_y",'&
        //'"'//trim(adjustl(name))//'_z"'
      end subroutine

      ! *****************************************************
      ! ********************* 2D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_2D_SF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Scalar Field"'
        write(u,'(A'//int2Str(sn+22)//')') 'VARIABLES = "X","Y",'& 
        //'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Header_2D_VF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Vector Field"'
        write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "X","Y",'& 
        //'"'//trim(adjustl(name))//'_x",'&
        //'"'//trim(adjustl(name))//'_y"'
      end subroutine

      subroutine exp_Header_2D_VF_dir(u,name,dir)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "2D Vector Field"'
        select case (dir)
        case (1); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "Y","Z",'& 
                  //'"'//trim(adjustl(name))//'_y",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (2); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "X","Z",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_z"'
        case (3); write(u,'(A'//int2Str(2*sn+29)//')') 'VARIABLES = "X","Y",'& 
                  //'"'//trim(adjustl(name))//'_x",'&
                  //'"'//trim(adjustl(name))//'_y"'
        case default; stop 'Error: dir must = 1,2,3 in exp_Header_2D in exp_Tec_Header.f90'
        end select
      end subroutine

      ! *****************************************************
      ! ********************* 1D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_1D_SF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "1D Scalar Field"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "X",'&
        //'"'//trim(adjustl(name))//'"'
      end subroutine

      subroutine exp_Header_0D_SF(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        integer :: sn
        sn = len(trim(adjustl(name)))
        write(u,'(A25)') 'TITLE = "0D Scalar Field"'
        write(u,'(A'//int2Str(sn+18)//')') 'VARIABLES = "N",'&
        //'"'//trim(adjustl(name))//'"'
      end subroutine

      end module