      module export_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      !
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use export_SF_mod
      use GF_export_mod
      use exp_Tecplot_Header_mod
      implicit none

      private
      public :: exp_3D_3C
      public :: exp_2D_2C
      public :: exp_2D_3C
      ! public :: exp_2D_1C ! Not developed yet, good for current coming out of page, e.g.

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3D_3C(m,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_3D_3C(un,name)
        do i=1,m%s
          call exp_3D_3C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF)
        enddo
      end subroutine

      subroutine exp_2D_2C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_2D_2C(un,dir,name)
        select case (dir)
        case(1)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%y%DL,i,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,2)
        enddo
        case(2)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,2)
        enddo
        case(3)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,2)
        enddo
        case default; stop 'Error: dir must = 1:3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine exp_2D_3C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_2D_3C(un,dir,name)
        do i=1,m%s
        call exp_2D_3C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,2)
        enddo
      end subroutine

      end module