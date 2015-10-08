      module import_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use mesh_mod
      use SF_mod
      use import_RF_mod
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
      public :: imp_3C_SF,imp_2C_SF,imp_1C_SF ! 3D Fields

      contains

      subroutine imp_3C_SF(m,pad,un,arrfmt,name,A,B,C)
        implicit none
        type(SF),intent(inout) :: A,B,C
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*);read(un,*) ! Read tecplot header
        do i=1,A%s
          call imp_3C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i),B%RF(i),C%RF(i))
        enddo
      end subroutine

      subroutine imp_2C_SF(m,pad,un,arrfmt,name,A,B)
        implicit none
        type(SF),intent(inout) :: A,B
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*);read(un,*) ! Read tecplot header
        do i=1,A%s
          call imp_2C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i),B%RF(i))
        enddo
      end subroutine

      subroutine imp_1C_SF(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i
        read(un,*);read(un,*);read(un,*) ! Read tecplot header
        do i=1,A%s
          call imp_1C_RF(m%g(i),pad,un,arrfmt,name,A%RF(i))
        enddo
      end subroutine

      end module