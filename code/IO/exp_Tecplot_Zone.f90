      module exp_Tecplot_Zone_mod
      use current_precision_mod
      use IO_tools_mod
      use datatype_conversion_mod
      implicit none

      private
      
      public :: exp_Zone_3I
      public :: exp_Zone_2I
      public :: exp_Zone_1I
      public :: exp_Zone_0I

      contains

      !*********************************************************************
      !************************ SCALAR FIELDS ******************************
      !*********************************************************************

      subroutine exp_Zone_3I(u,s,t)
        implicit none
        integer,intent(in),dimension(3) :: s
        integer,intent(in) :: u,t
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t))))) // & 
                ',A,A6,A'  // int2str(len(trim(adjustl(int2str(s(1)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(2)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(3)))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',trim(adjustl(int2str(t))),'"', &
                ', I = ',trim(adjustl(int2str(s(1)))), &
                ', J = ',trim(adjustl(int2str(s(2)))), &
                ', K = ',trim(adjustl(int2str(s(3)))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine exp_Zone_2I(u,s,t)
        implicit none
        integer,intent(in) :: u,t
        integer,dimension(2),intent(in) :: s
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t))))) // & 
                ',A,A6,A'  // int2str(len(trim(adjustl(int2str(s(1)))))) // & 
                ',A6,A'    // int2str(len(trim(adjustl(int2str(s(2)))))) // & 
                ',A20)') 'ZONE ', &
                ', T ="',trim(adjustl(int2str(t))),'"', &
                ', I = ',trim(adjustl(int2str(s(1)))), &
                ', J = ',trim(adjustl(int2str(s(2)))), &
                ' DATAPACKING = POINT'
      end subroutine

      subroutine exp_Zone_1I(u,s,t)
        implicit none
        integer,intent(in) :: s,u,t
        write(u,'(A5,A6,A' // int2str(len(trim(adjustl(int2str(t))))) // & 
                ',A,A6,A'  // int2str(len(trim(adjustl(int2str(s))))) // & 
                ',A20)') 'ZONE ',&
                ', T ="',trim(adjustl(int2str(t))),'"',&
                ', I = ',trim(adjustl(int2str(s))),&
                ' DATAPACKING = POINT'
      end subroutine

      subroutine exp_Zone_0I(u)
        implicit none
        integer,intent(in) :: u
        write(u,'(A24)') 'ZONE DATAPACKING = POINT'
      end subroutine

      end module