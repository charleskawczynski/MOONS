      module export_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use mesh_mod
      use SF_mod
      use export_RF_mod
      use IO_tecplotHeaders_mod
      use exp_Tecplot_Zone_mod
      use exp_Tecplot_Header_mod
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
      public :: exp_mesh_SF

      contains

      ! public :: exp_Header_3D_SF(u,name)
      ! public :: exp_Header_3D_VF(u,name)
      ! public :: exp_Header_2D_SF(u,name)
      ! public :: exp_Header_2D_VF(u,name)
      ! public :: exp_Header_2D_VF_dir(u,name,dir)
      ! public :: exp_Header_1D_SF(u,name)
      ! public :: exp_Header_0D_SF(u,name)
      ! public :: exp_Zone_3I(u,s,t)
      ! public :: exp_Zone_2I(u,s,t)
      ! public :: exp_Zone_1I(u,s,t)
      ! public :: exp_Zone_0I(u,s,t)

      subroutine exp_3C_SF(m,pad,un,arrfmt,name,A,B,C)
        implicit none
        type(SF),intent(in) :: A,B,C
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        ! Pad needs to be included somehow
        call exp_Header_3D_VF(un,name)
        do i=1,m%s
          call exp_Zone_3I(un,A%RF(i)%s-2*pad,i)
          call exp_3C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i),B%RF(i),C%RF(i))
        enddo
      end subroutine

      subroutine exp_2C_SF(m,pad,un,arrfmt,name,A,B)
        implicit none
        type(SF),intent(in) :: A,B
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        integer,dimension(2) :: s
        s = (/m%N_cells(1),m%N_cells(2)/)
        ! Pad needs to be included somehow, this needs to be fixed, big time...
        call writeTecPlotHeader(un,name//'x',name//'y',s(1)-2*pad,s(2)-2*pad)
        do i=1,m%s
          call exp_2C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i),B%RF(i))
        enddo
      end subroutine

      subroutine exp_1C_SF(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(in) :: A
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        ! Pad needs to be included somehow
        ! call writeTecPlotHeader(un,name,m%N_cells+2-2*pad)
        call exp_Header_3D_SF(un,name)
        do i=1,m%s
          call exp_Zone_3I(un,A%RF(i)%s-2*pad,i)
          call exp_1C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i))
        enddo
      end subroutine

      subroutine exp_mesh_SF(m,pad,un,arrfmt,name)
        implicit none
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(3) :: s
        integer :: i
        s = m%N_cells
        ! Pad needs to be included somehow
        ! The MESH size needs to be passed somehow, maybe
        ! set and pass props in mesh init.
        call writeTecPlotHeader(un,name,s-2*pad)
        do i=1,m%s
          call exp_grid_RF(m%g(i),pad,un,arrfmt)
        enddo
      end subroutine

      end module