      module export_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use grid_mod
      use VF_mod
      use export_mod
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

      public :: exp_3D_3Cg_VF,exp_3D_2Cg_VF,exp_3D_1Cg_VF ! 3D Fields

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3D_3Cg_VF(g,DT,pad,un,arrfmt,s,U)
        implicit none
        type(VF),intent(in) :: U
        type(grid),intent(in) :: g
        integer,intent(in) :: DT,pad,un
        integer,dimension(3),intent(in) :: s
        character(len=*),intent(in) :: arrfmt
        call exp_3D_3Cg(g,DT,pad,un,arrfmt,s,U%x,U%y,U%z)
      end subroutine

      subroutine exp_3D_2Cg_VF(g,DT,pad,un,arrfmt,comp,s,U)
        implicit none
        type(VF),intent(in) :: U
        type(grid),intent(in) :: g
        integer,intent(in) :: DT,pad,un,comp
        integer,dimension(3),intent(in) :: s
        character(len=*),intent(in) :: arrfmt
        select case (comp)
        case (1); call exp_3D_2Cg(g,DT,pad,un,arrfmt,s,U%y,U%z)
        case (2); call exp_3D_2Cg(g,DT,pad,un,arrfmt,s,U%x,U%z)
        case (3); call exp_3D_2Cg(g,DT,pad,un,arrfmt,s,U%x,U%y)
        case default
        stop 'Error: comp must = 1,2,3 in exp_3D_2Cg_VF in export_VF.f90'
        end select
      end subroutine

      subroutine exp_3D_1Cg_VF(g,DT,pad,un,arrfmt,comp,s,u)
        implicit none
        type(VF),intent(in) :: U
        type(grid),intent(in) :: g
        integer,intent(in) :: DT,pad,un,comp
        integer,dimension(3),intent(in) :: s
        character(len=*),intent(in) :: arrfmt
        select case (comp)
        case (1); call exp_3D_1Cg(g,DT,pad,un,arrfmt,s,U%x)
        case (2); call exp_3D_1Cg(g,DT,pad,un,arrfmt,s,U%y)
        case (3); call exp_3D_1Cg(g,DT,pad,un,arrfmt,s,U%z)
        case default
        stop 'Error: comp must = 1,2,3 in exp_3D_1Cg_VF in export_VF.f90'
        end select
      end subroutine

      end module