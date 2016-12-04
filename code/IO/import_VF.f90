      module import_VF_mod
      ! This module, along with importRaw.f90 provide purely functional
      ! pipeline routines to import data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      !
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use GF_import_mod
      use import_SF_mod
      implicit none

      private
      public :: imp_3D_3C
      public :: imp_2D_2C

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_3D_3C(m,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*) ! Read tecplot header
        do i=1,m%s
          call imp_3D_3C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF)
        enddo
      end subroutine

      subroutine imp_2D_2C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*) ! Read tecplot header

        select case (dir)
        case(1)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%y%DL,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,2)
        enddo
        case(2)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,2)
        enddo
        case(3)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,2)
        enddo
        case default; stop 'Error: dir must = 1,2,3 in imp_2D_2C in import_SF.f90'
        end select
      end subroutine

      end module