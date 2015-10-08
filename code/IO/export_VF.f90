      module export_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use mesh_mod
      use VF_mod
      use export_SF_mod
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
      public :: exp_3C_VF,exp_2C_VF,exp_1C_VF ! 3D Fields

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3C_VF(m,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        call exp_3C_SF(m,pad,un,arrfmt,name,U%x,U%y,U%z)
      end subroutine

      subroutine exp_2C_VF(m,pad,un,arrfmt,name,comp,U)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,comp
        character(len=*),intent(in) :: arrfmt,name
        select case (comp)
        case (1); call exp_2C_SF(m,pad,un,arrfmt,name,U%y,U%z)
        case (2); call exp_2C_SF(m,pad,un,arrfmt,name,U%x,U%z)
        case (3); call exp_2C_SF(m,pad,un,arrfmt,name,U%x,U%y)
        case default
        stop 'Error: comp must = 1,2,3 in exp_2Cg_VF in export_VF.f90'
        end select
      end subroutine

      subroutine exp_1C_VF(m,pad,un,arrfmt,name,comp,u)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,comp
        character(len=*),intent(in) :: arrfmt,name
        select case (comp)
        case (1); call exp_1C_SF(m,pad,un,arrfmt,name,U%x)
        case (2); call exp_1C_SF(m,pad,un,arrfmt,name,U%y)
        case (3); call exp_1C_SF(m,pad,un,arrfmt,name,U%z)
        case default
        stop 'Error: comp must = 1,2,3 in exp_1Cg_VF in export_VF.f90'
        end select
      end subroutine

      end module