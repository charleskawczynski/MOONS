      module export_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible grid types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use mesh_mod
      use SF_mod
      use export_g_mod
      use exp_Tecplot_Zone_mod
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
        integer :: i,DT
        call exp_Header_3D_1C(un,name)
        DT = getType_3D(m%g(1),A%RF(1)%s,name)
        do i=1,m%s
          call exp_Zone_3I(un,A%RF(i)%s-2*pad,i)
          call exp_3D_1C_g(m%g(i),DT,pad,un,arrfmt,A%RF(i)%s,A%RF(i)%f)
        enddo
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
        integer,dimension(2) :: s
        integer :: i,DT
        call exp_Header_2D_1C(un,name,dir)
        select case (dir)
        case(1); s = (/A%RF(1)%s(2),A%RF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(2); s = (/A%RF(1)%s(1),A%RF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(3); s = (/A%RF(1)%s(1),A%RF(1)%s(2)/); DT = getType_2D(m%g(1),s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   s = (/A%RF(i)%s(2),A%RF(i)%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   s = (/A%RF(i)%s(1),A%RF(i)%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   s = (/A%RF(i)%s(1),A%RF(i)%s(2)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%RF(i)%f(:,:,2))
                 enddo
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine exp_mesh_SF(m,pad,un,arrfmt,name)
        implicit none
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(3) :: s
        integer :: i,j
        call exp_Header_3D_1C(un,name)
        do i=1,m%s
          s = (/(m%g(i)%c(j)%sn,j=1,3)/)
          call exp_Zone_3I(un,s-2*pad,i)
          call exp_3D_1C_S_g(m%g(i),1,pad,un,arrfmt,s,1.0_cp)
        enddo
      end subroutine

      end module