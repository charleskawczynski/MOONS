      module export_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use grid_mod
      use SF_mod
      use export_RF_mod
      use IO_tecplotHeaders_mod
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
      public :: exp_3C_SF,exp_2C_SF,exp_1C_SF ! 3D Fields
      public :: exp_grid_SF

      contains

      subroutine exp_3C_SF(g,pad,un,arrfmt,name,A,B,C)
        implicit none
        type(SF),intent(in) :: A,B,C
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        ! Pad needs to be included somehow
        call writeTecPlotHeader(un,name//'x',name//'y',name//'z',A%stot-2*pad)
        do i=1,A%s
          call exp_3C_RF(g,pad,un,arrfmt,name,A%RF(i),B%RF(i),C%RF(i))
        enddo
      end subroutine

      subroutine exp_2C_SF(g,pad,un,arrfmt,name,A,B)
        implicit none
        type(SF),intent(in) :: A,B
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        integer,dimension(2) :: s
        s = (/g%c(1)%sn,g%c(2)%sn/)
        ! Pad needs to be included somehow, this needs to be fixed, big time...
        call writeTecPlotHeader(un,name//'x',name//'y',s(1)-2*pad,s(2)-2*pad)
        do i=1,A%s
          call exp_2C_RF(g,pad,un,arrfmt,name,A%RF(i),B%RF(i))
        enddo
      end subroutine

      subroutine exp_1C_SF(g,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(in) :: A
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        ! Pad needs to be included somehow
        call writeTecPlotHeader(un,name,A%stot-2*pad)
        do i=1,A%s
          call exp_1C_RF(g,pad,un,arrfmt,name,A%RF(i))
        enddo
      end subroutine

      subroutine exp_grid_SF(g,pad,un,arrfmt,name)
        implicit none
        type(grid),intent(in) :: g
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(3) :: s
        s = (/g%c(1)%sn,g%c(2)%sn,g%c(3)%sn/)
        ! Pad needs to be included somehow
        call writeTecPlotHeader(un,name,s-2*pad)
        ! do i=1,size(g)
          call exp_grid_RF(g,pad,un,arrfmt)
        ! enddo
      end subroutine

      end module