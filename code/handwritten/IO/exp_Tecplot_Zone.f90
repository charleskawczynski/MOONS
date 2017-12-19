      module exp_Tecplot_Zone_mod
      use current_precision_mod
      use string_mod
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
        type(string) :: st
        call init(st,'ZONE ')
        call append(st,', T ="'//int2str(t)//'"')
        call append(st,', I ='//int2str(s(1)))
        call append(st,', J ='//int2str(s(2)))
        call append(st,', K ='//int2str(s(3)))
        call append(st,' DATAPACKING = POINT')
        write(u,*) str(st)
        call delete(st)
      end subroutine

      subroutine exp_Zone_2I(u,s,t)
        implicit none
        integer,intent(in) :: u,t
        integer,dimension(2),intent(in) :: s
        type(string) :: st
        call init(st,'ZONE ')
        call append(st,', T ="'//int2str(t)//'"')
        call append(st,', I ='//int2str(s(1)))
        call append(st,', J ='//int2str(s(2)))
        call append(st,' DATAPACKING = POINT')
        write(u,*) str(st)
        call delete(st)
      end subroutine

      subroutine exp_Zone_1I(u,s,t)
        implicit none
        integer,intent(in) :: s,u,t
        type(string) :: st
        call init(st,'ZONE ')
        call append(st,', T ="'//int2str(t)//'"')
        call append(st,', I ='//int2str(s))
        call append(st,' DATAPACKING = POINT')
        write(u,*) str(st)
        call delete(st)
      end subroutine

      subroutine exp_Zone_0I(u)
        implicit none
        integer,intent(in) :: u
        write(u,*) 'ZONE DATAPACKING = POINT'
      end subroutine

      end module