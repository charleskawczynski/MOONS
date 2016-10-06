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
        DT = getType_3D(m%B(1)%g,A%BF(1)%GF%s,name)
        do i=1,m%s
          call exp_Zone_3I(un,A%BF(i)%GF%s-2*pad,i)
          call exp_3D_1C_g(m%B(i)%g,DT,pad,un,arrfmt,A%BF(i)%GF%s,A%BF(i)%GF%f)
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
        call exp_Header_2D_1C(un,dir,name)
        select case (dir)
        case(1); s = (/A%BF(1)%GF%s(2),A%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(2); s = (/A%BF(1)%GF%s(1),A%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(3); s = (/A%BF(1)%GF%s(1),A%BF(1)%GF%s(2)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   s = (/A%BF(i)%GF%s(2),A%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,A%BF(i)%GF%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   s = (/A%BF(i)%GF%s(1),A%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,A%BF(i)%GF%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   s = (/A%BF(i)%GF%s(1),A%BF(i)%GF%s(2)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_1C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,A%BF(i)%GF%f(:,:,2))
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
          s = (/(m%B(i)%g%c(j)%sn,j=1,3)/)
          call exp_Zone_3I(un,s-2*pad,i)
          call exp_3D_1C_S_g(m%B(i)%g,1,pad,un,arrfmt,s,1.0_cp)
        enddo
      end subroutine

      end module