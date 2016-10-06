      module IO_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use export_raw_mod
      use coordinates_mod
      use SF_mod
      implicit none

      private
      public :: export_3D_3C
      public :: export_2D_3C
      public :: export_1D_3C

      contains

      subroutine export_3D_3C(m,U,dir,name,pad)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: U
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: pad
        integer :: un
        un = new_and_open(dir,name)
        call exp_Header_3D_3C(un,name)
        do i=1,m%s
          call exp_Zone_3I(un,U%x%BF(i)%GF%s-2*pad,i)
          call exp_3D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                         U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                         get_h(U%x,m%B(i)%g%c(1),U%x%BF(i)%GF%s(1)),&
                         get_h(U%x,m%B(i)%g%c(2),U%x%BF(i)%GF%s(2)),&
                         get_h(U%x,m%B(i)%g%c(3),U%x%BF(i)%GF%s(3)),&
                         fmt,pad,un)
        enddo
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_2D_3C(m,U,dir,name,pad,direction)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: U
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: pad,direction
        integer :: un
        un = new_and_open(dir,name)
        call exp_Header_2D_3C(un,name)
        select case (direction)
        case (1)
          do i=1,m%s
            call exp_Zone_2I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_2D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(2),U%x%BF(i)%GF%s(2)),&
                           get_h(U%x,m%B(i)%g%c(3),U%x%BF(i)%GF%s(3)),&
                           fmt,pad,un)
          enddo
        case (2)
          do i=1,m%s
            call exp_Zone_2I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_2D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(1),U%x%BF(i)%GF%s(1)),&
                           get_h(U%x,m%B(i)%g%c(3),U%x%BF(i)%GF%s(3)),&
                           fmt,pad,un)
          enddo
        case (3)
          do i=1,m%s
            call exp_Zone_2I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_2D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(1),U%x%BF(i)%GF%s(1)),&
                           get_h(U%x,m%B(i)%g%c(2),U%x%BF(i)%GF%s(2)),&
                           fmt,pad,un)
          enddo
        case default; stop 'Error: direction must = 1,2,3 in export_2D_3C in IO_VF.f90'
        end select
        call close_and_message(un,dir,name)
      end subroutine

      subroutine export_1D_3C(m,U,dir,name,pad,axis)
        implicit none
        type(mesh),intent(in) :: m
        type(VF),intent(in) :: U
        character(len=*),intent(in) :: dir,name
        integer,intent(in) :: pad,axis
        integer :: un
        un = new_and_open(dir,name)
        call exp_Header_1D_3C(un,name)
        select case (axis)
        case (1)
          do i=1,m%s
            call exp_Zone_1I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_1D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(1),U%x%BF(i)%GF%s(1)),&
                           fmt,pad,un)
          enddo
        case (2)
          do i=1,m%s
            call exp_Zone_1I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_1D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(2),U%x%BF(i)%GF%s(2)),&
                           fmt,pad,un)
          enddo
        case (3)
          do i=1,m%s
            call exp_Zone_1I(un,U%x%BF(i)%GF%s-2*pad,i)
            call exp_1D_3C(U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3),&
                           U%x%BF(i)%GF%f   ,U%y%BF(i)%GF%f   ,U%z%BF(i)%GF%f,&
                           get_h(U%x,m%B(i)%g%c(3),U%x%BF(i)%GF%s(3)),&
                           fmt,pad,un)
          enddo
        case default; stop 'Error: axis must = 1,2,3 in export_1D_3C in IO_VF.f90'
        end select
        call close_and_message(un,dir,name)
      end subroutine

      ! ***********************************************************************
      ! ************************* GET COORDNIATES *****************************
      ! ***********************************************************************

      function get_h(U,c,s) result(h)
        implicit none
        type(SF),intent(in) :: U
        type(coordinates),intent(in) :: c
        integer,intent(in) :: s
        real(cp),dimension(s) :: h
            if (U%N_along(1)) then; h = g%hn
        elseif (U%C_along(1)) then; h = g%hc
        else; stop 'Error: bad data  input to get_h in export_g.f90'
        endif
      end function

      end module