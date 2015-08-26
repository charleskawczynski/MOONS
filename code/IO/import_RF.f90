      module import_RF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use grid_mod
      use RF_mod
      use import_g_mod
      implicit none

#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)
#endif
#ifdef _DOUBLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(14)
#endif
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32)
#endif

      private

      public :: imp_3C_RF,imp_2C_RF,imp_1C_RF ! 3D Fields

      contains

      subroutine imp_3C_RF(g,pad,un,arrfmt,name,A,B,C)
        implicit none
        type(realField),intent(inout) :: A,B,C
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call imp_3D_3C_g(g,DT,pad,un,arrfmt,A%s,A%f,B%f,C%f)
      end subroutine

      subroutine imp_2C_RF(g,pad,un,arrfmt,name,A,B)
        implicit none
        type(realField),intent(inout) :: A,B
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call imp_3D_2C_g(g,DT,pad,un,arrfmt,A%s,A%f,B%f)
      end subroutine

      subroutine imp_1C_RF(g,pad,un,arrfmt,name,A)
        implicit none
        type(realField),intent(inout) :: A
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call imp_3D_1C_g(g,DT,pad,un,arrfmt,A%s,A%f)
      end subroutine

      end module