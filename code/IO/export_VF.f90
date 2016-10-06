      module export_VF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use export_SF_mod
      use export_g_mod
      use exp_Tecplot_Zone_mod
      use exp_Tecplot_Header_mod
      implicit none

      private
      public :: exp_3D_3C
      public :: exp_2D_2C
      public :: exp_2D_3C
      ! public :: exp_2D_1C ! Not developed yet, good for current coming out of page, e.g.

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine exp_3D_3C(m,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i,DT
        call exp_Header_3D_3C(un,name)
        DT = getType_3D(m%B(1)%g,U%x%BF(1)%GF%s,name)
        do i=1,m%s
          call exp_Zone_3I(un,U%x%BF(i)%GF%s-2*pad,i)
          call exp_3D_3C_g(m%B(i)%g,DT,pad,un,arrfmt,U%x%BF(i)%GF%s,U%x%BF(i)%GF%f,U%y%BF(i)%GF%f,U%z%BF(i)%GF%f)
        enddo
      end subroutine

      subroutine exp_2D_2C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(2) :: s
        integer :: i,DT
        call exp_Header_2D_2C(un,dir,name)
        select case (dir)
        case(1); s = (/U%x%BF(1)%GF%s(2),U%x%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(2); s = (/U%x%BF(1)%GF%s(1),U%x%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(3); s = (/U%x%BF(1)%GF%s(1),U%x%BF(1)%GF%s(2)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   s = (/U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_2C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%y%BF(i)%GF%f(2,:,:),U%z%BF(i)%GF%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   s = (/U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_2C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%x%BF(i)%GF%f(:,2,:),U%z%BF(i)%GF%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   s = (/U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2)/); call exp_Zone_2I(un,s-2*pad,i)
                   call exp_2D_2C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%x%BF(i)%GF%f(:,:,2),U%y%BF(i)%GF%f(:,:,2))
                 enddo
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine exp_2D_3C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(2) :: s
        integer :: i,DT
        call exp_Header_2D_3C(un,dir,name)
        select case (dir)
        case(1); s = (/U%x%BF(1)%GF%s(2),U%x%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(2); s = (/U%x%BF(1)%GF%s(1),U%x%BF(1)%GF%s(3)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case(3); s = (/U%x%BF(1)%GF%s(1),U%x%BF(1)%GF%s(2)/); DT = getType_2D(m%B(1)%g,s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_3C in export_SF.f90'
        end select
        select case (dir)
        case(1)
        do i=1,m%s
          s = (/U%x%BF(i)%GF%s(2),U%x%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
          call exp_2D_3C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%x%BF(i)%GF%f(2,:,:),U%y%BF(i)%GF%f(2,:,:),U%z%BF(i)%GF%f(2,:,:))
        enddo
        case(2)
        do i=1,m%s
          s = (/U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(3)/); call exp_Zone_2I(un,s-2*pad,i)
          call exp_2D_3C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%x%BF(i)%GF%f(:,2,:),U%y%BF(i)%GF%f(:,2,:),U%z%BF(i)%GF%f(:,2,:))
        enddo
        case(3)
        do i=1,m%s
          s = (/U%x%BF(i)%GF%s(1),U%x%BF(i)%GF%s(2)/); call exp_Zone_2I(un,s-2*pad,i)
          call exp_2D_3C_g(m%B(i)%g,DT,pad,un,arrfmt,s,dir,U%x%BF(i)%GF%f(:,:,2),U%y%BF(i)%GF%f(:,:,2),U%z%BF(i)%GF%f(:,:,2))
        enddo
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_3C in export_SF.f90'
        end select
      end subroutine

      end module