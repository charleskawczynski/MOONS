      module export_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      !
      use current_precision_mod
      use mesh_mod
      use data_location_mod
      use SF_mod
      use GF_export_mod
      use exp_Tecplot_Header_mod
      implicit none

      private
      public :: exp_3D_1C
      public :: exp_2D_1C
      public :: exp_mesh_SF

      contains

      ! ******************************************************************
      ! ******************************* 3D *******************************
      ! ******************************************************************

      subroutine exp_3D_1C(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(in) :: A
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_3D_1C(un,name)
        do i=1,m%s; call exp_3D_1C_GF(m%B(i)%g,A%DL,i,pad,un,A%BF(i)%GF); enddo
      end subroutine

      ! ******************************************************************
      ! ******************************* 2D *******************************
      ! ******************************************************************

      subroutine exp_2D_1C(m,pad,un,arrfmt,name,A,dir)
        implicit none
        type(SF),intent(in) :: A
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_2D_1C(un,dir,name)
        do i=1,m%s; call exp_2D_1C_GF(m%B(i)%g,A%DL,i,pad,un,A%BF(i)%GF,dir,2); enddo
      end subroutine

      subroutine exp_mesh_SF(m,pad,un,arrfmt,name)
        implicit none
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        call exp_Header_3D_1C(un,name)
        do i=1,m%s; call exp_3D_0C_GF(m%B(i)%g,DL_Node(),i,pad,un,1.0_cp); enddo
      end subroutine

      end module