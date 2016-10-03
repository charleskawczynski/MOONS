      module import_VF_mod
      ! This module, along with importRaw.f90 provide purely functional 
      ! pipeline routines to import data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use mesh_mod
      use VF_mod
      use import_g_mod
      use import_SF_mod
      implicit none

      private
      public :: imp_3D_3C
      public :: imp_2D_2C

      contains

      ! ***********************************************************************
      ! ***********************************************************************
      ! ****************************** 3D FIELDS ******************************
      ! ***********************************************************************
      ! ***********************************************************************

      subroutine imp_3D_3C(m,pad,un,arrfmt,name,U)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        DT = getType_3D(m%g(1),U%x%GF(1)%s,name)
        do i=1,m%s
          read(un,*) ! Read tecplot header
          call imp_3D_3C_g(m%g(i),DT,pad,un,arrfmt,U%x%GF(i)%s,U%x%GF(i)%f,U%y%GF(i)%f,U%z%GF(i)%f)
        enddo
      end subroutine

      subroutine imp_2D_2C(m,pad,un,arrfmt,name,U,dir)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(2) :: s
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        select case (dir)
        case(1); s = (/U%x%GF(1)%s(2),U%x%GF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(2); s = (/U%x%GF(1)%s(1),U%x%GF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(3); s = (/U%x%GF(1)%s(1),U%x%GF(1)%s(2)/); DT = getType_2D(m%g(1),s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in imp_2D_2C in import_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   s = (/U%x%GF(i)%s(2),U%x%GF(i)%s(3)/); read(un,*)
                   call imp_2D_2C_g(m%g(i),DT,pad,un,arrfmt,s,dir,U%y%GF(i)%f(2,:,:),U%z%GF(i)%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   s = (/U%x%GF(i)%s(1),U%x%GF(i)%s(3)/); read(un,*)
                   call imp_2D_2C_g(m%g(i),DT,pad,un,arrfmt,s,dir,U%x%GF(i)%f(:,2,:),U%z%GF(i)%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   s = (/U%x%GF(i)%s(1),U%x%GF(i)%s(2)/); read(un,*)
                   call imp_2D_2C_g(m%g(i),DT,pad,un,arrfmt,s,dir,U%x%GF(i)%f(:,:,2),U%y%GF(i)%f(:,:,2))
                 enddo
        case default; stop 'Error: dir must = 1,2,3 in imp_2D_2C in import_SF.f90'
        end select
      end subroutine

      end module