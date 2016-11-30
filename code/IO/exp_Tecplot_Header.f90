      module exp_Tecplot_Header_mod
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
        write(u,*) 'TITLE = "3D Scalar Field"'
        write(u,*) 'VARIABLES = "x","y","z","'//name//'"'
      end subroutine

      subroutine exp_Header_3D_3C(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        write(u,*) 'TITLE = "3D Vector Field"'
        write(u,*) 'VARIABLES = "x","y","z","'//name//'_x","'//name//'_y","'//name//'_z"'
      end subroutine

      ! *****************************************************
      ! ********************* 2D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_2D_1C(u,dir,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        write(u,*) 'TITLE = "2D Scalar Field"'
        select case (dir)
        case (1); write(u,*) 'VARIABLES = "y","z","'//name//'_x"'
        case (2); write(u,*) 'VARIABLES = "x","z","'//name//'_y"'
        case (3); write(u,*) 'VARIABLES = "x","y","'//name//'_z"'
        case default; stop 'Error: dir must=1:3 in exp_Header_2D_1C in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_2D_2C(u,dir,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        write(u,*) 'TITLE = "2D Vector Field"'
        select case (dir)
        case (1); write(u,*) 'VARIABLES = "y","z","'//name//'_y","'//name//'_z"'
        case (2); write(u,*) 'VARIABLES = "x","z","'//name//'_x","'//name//'_z"'
        case (3); write(u,*) 'VARIABLES = "x","y","'//name//'_x","'//name//'_y"'
        case default; stop 'Error: dir must=1:3 in exp_Header_2D_2C in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_2D_3C(u,dir,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        write(u,*) 'TITLE = "2D Vector Field"'
        select case (dir)
        case (1); write(u,*) 'VARIABLES = "y","z","'//name//'_x","'//name//'_y","'//name//'_z"'
        case (2); write(u,*) 'VARIABLES = "x","z","'//name//'_x","'//name//'_y","'//name//'_z"'
        case (3); write(u,*) 'VARIABLES = "x","y","'//name//'_x","'//name//'_y","'//name//'_z"'
        case default; stop 'Error: dir must=1:3 in exp_Header_2D_3C in exp_Tec_Header.f90'
        end select
      end subroutine

      ! *****************************************************
      ! ********************* 1D FIELDS *********************
      ! *****************************************************

      subroutine exp_Header_1D_1C(u,dir,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u,dir
        write(u,*) 'TITLE = "1D Scalar Field"'
        select case (dir)
        case (1); write(u,*) 'VARIABLES = "x","'//name//'"'
        case (2); write(u,*) 'VARIABLES = "y","'//name//'"'
        case (3); write(u,*) 'VARIABLES = "z","'//name//'"'
        case default; stop 'Error: dir must=1:3 in exp_Header_1D_1C in exp_Tec_Header.f90'
        end select
      end subroutine

      subroutine exp_Header_0D_1C(u,name)
        implicit none
        character(len=*),intent(in) :: name
        integer,intent(in) :: u
        write(u,*) 'TITLE = "0D Scalar Field"'
        write(u,*) 'VARIABLES = "N","'//name//'"'
      end subroutine

      end module