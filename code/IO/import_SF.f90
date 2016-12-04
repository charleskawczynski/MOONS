      module import_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      !
      use current_precision_mod
      use IO_tools_mod
      use mesh_mod
      use SF_mod
      use GF_import_mod
      use string_mod
      use string_aux_mod
      implicit none

      private
      public :: imp_3D_1C
      public :: imp_2D_1C

      contains

      subroutine imp_3D_1C(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*) ! Read tecplot header
        do i=1,m%s; call imp_3D_1C_GF(m%B(i)%g,A%DL,i,pad,un,A%BF(i)%GF); enddo
      end subroutine

      subroutine imp_2D_1C(m,pad,un,arrfmt,name,A,dir)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*) ! Read tecplot header
        do i=1,m%s; call imp_2D_1C_GF(m%B(i)%g,A%DL,i,pad,un,A%BF(i)%GF,dir,2); enddo
      end subroutine

      end module