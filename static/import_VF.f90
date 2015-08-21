      module import_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use grid_mod
      use VF_mod
      use import_SF_mod
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

      public :: imp_3C_VF,imp_2C_VF,imp_1C_VF ! 3D Fields

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_3C_VF(g,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(inout) :: U
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        call imp_3C_SF(g,pad,un,arrfmt,name,U%x,U%y,U%z)
      end subroutine

      subroutine imp_2C_VF(g,pad,un,arrfmt,name,comp,U)
        implicit none
        type(VF),intent(inout) :: U
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un,comp
        character(len=*),intent(in) :: arrfmt,name
        select case (comp)
        case (1); call imp_2C_SF(g,pad,un,arrfmt,name,U%y,U%z)
        case (2); call imp_2C_SF(g,pad,un,arrfmt,name,U%x,U%z)
        case (3); call imp_2C_SF(g,pad,un,arrfmt,name,U%x,U%y)
        case default
        stop 'Error: comp must = 1,2,3 in imp_2Cg_VF in export_VF.f90'
        end select
      end subroutine

      subroutine imp_1C_VF(g,pad,un,arrfmt,name,comp,u)
        implicit none
        type(VF),intent(inout) :: U
        type(grid),intent(inout) :: g
        integer,intent(in) :: pad,un,comp
        character(len=*),intent(in) :: arrfmt,name
        select case (comp)
        case (1); call imp_1C_SF(g,pad,un,arrfmt,name,U%x)
        case (2); call imp_1C_SF(g,pad,un,arrfmt,name,U%y)
        case (3); call imp_1C_SF(g,pad,un,arrfmt,name,U%z)
        case default
        stop 'Error: comp must = 1,2,3 in imp_1Cg_VF in export_VF.f90'
        end select
      end subroutine

      end module