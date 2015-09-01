      module export_RF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use grid_mod
      use RF_mod
      use export_g_mod
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

      public :: exp_3C_RF,exp_2C_RF,exp_1C_RF ! 3D Fields
      public :: exp_grid_RF

      contains

      subroutine exp_3C_RF(g,pad,un,arrfmt,name,A,B,C)
        implicit none
        type(realField),intent(in) :: A,B,C
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call exp_3D_3C_g(g,DT,pad,un,arrfmt,A%s,A%f,B%f,C%f)
      end subroutine

      subroutine exp_2C_RF(g,pad,un,arrfmt,name,A,B)
        implicit none
        type(realField),intent(in) :: A,B
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call exp_3D_2C_g(g,DT,pad,un,arrfmt,A%s,A%f,B%f)
      end subroutine

      subroutine exp_1C_RF(g,pad,un,arrfmt,name,A)
        implicit none
        type(realField),intent(in) :: A
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: DT
        DT = getType_3D(g,A%s,name)
        call exp_3D_1C_g(g,DT,pad,un,arrfmt,A%s,A%f)
      end subroutine

      subroutine exp_grid_RF(g,pad,un,arrfmt)
        implicit none
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt
        integer,dimension(3) :: s
        s = (/g%c(1)%sn,g%c(2)%sn,g%c(3)%sn/)
        call exp_3D_1C_S_g(g,1,pad,un,arrfmt,s,1.0_cp)
      end subroutine

      end module