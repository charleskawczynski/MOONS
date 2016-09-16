      module exp_Tecplot_Headers_mod
      use current_precision_mod
      use string_mod
      use string_fmt_mod
      use IO_tools_mod
      use datatype_conversion_mod
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

      subroutine init_Header_3D_1C(FH,s,t,name)
        implicit none
        type(file_header),intent(in) :: H
        character(len=*),intent(in) :: name
        integer,dimension(3),intent(in) :: s
        integer,intent(in) :: t
        call init(FH%title%s,'TITLE = "3D Scalar Field"')
        call init(FH%title%fmt,'(A25)')

        call init(FH%variables%s,'VARIABLES = "x","y","z",'//'"'//name//'"')
        call init(FH%variables%fmt,'(A'//int2Str(len(name)+26)//')')

        call init(FH%zone%s,'ZONE ')
        call append(FH%zone%s,', T ="',int2str(t),'"')
        call append(FH%zone%s,', I = ',int2str(s(1)))
        call append(FH%zone%s,', J = ',int2str(s(2)))
        call append(FH%zone%s,', K = ',int2str(s(3)))
        call append(FH%zone%s,' DATAPACKING = POINT')

        call append(FH%zone%fmt'(A5,A6,A' // int2str(len(int2str(t))))
        call append(FH%zone%fmt',A,A6,A'  // int2str(len(int2str(s(1))))) 
        call append(FH%zone%fmt',A6,A'    // int2str(len(int2str(s(2)))))
        call append(FH%zone%fmt',A6,A'    // int2str(len(int2str(s(3)))))
        call append(FH%zone%fmt',A20)')
      end subroutine

      subroutine export_file_header(FH)
        implicit none
        type(file_header),intent(in) :: FH
        write(FH%un,FH%title%fmt) FH%title%s
        write(FH%un,FH%variables%fmt) FH%variables%s
        write(FH%un,FH%zone%fmt) FH%zone%s
      end subroutine

      subroutine export_file_header(FH)
        implicit none
        type(file_header),intent(in) :: FH
        write(FH%un,FH%title%fmt) FH%title%s
        write(FH%un,FH%variables%fmt) FH%variables%s
        write(FH%un,FH%zone%fmt) FH%zone%s
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

      subroutine exp_Header_2D_1C(u,dir,name)
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

      subroutine exp_Header_2D_2C(u,dir,name)
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

      subroutine exp_Header_2D_3C(u,dir,name)
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

      subroutine exp_Header_1D_1C(u,dir,name)
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