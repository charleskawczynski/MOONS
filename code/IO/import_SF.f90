      module import_SF_mod
      ! This module, along with exportRaw.f90 provide purely functional 
      ! pipeline routines to export data given inputs. The possible mesh types
      ! can be checked in the getType_3D,getType_2D,getType_1D routines.
      ! 
      use current_precision_mod
      use IO_tools_mod
      use mesh_mod
      use SF_mod
      use import_g_mod
      use string_mod
      use string_aux_mod
      implicit none

      private
      public :: imp_3D_1C
      public :: imp_2D_1C

      contains

      subroutine imp_3D_1C(m,pad,un,arrfmt,name,A)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: arrfmt,name
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        DT = getType_3D(m%g(1),A%GF(1)%s,name)
        do i=1,m%s
          read(un,*) ! Read tecplot header
          call imp_3D_1C_g(m%g(i),DT,pad,un,arrfmt,A%GF(i)%s,A%GF(i)%f)
        enddo
      end subroutine

      subroutine imp_2D_1C(m,pad,un,arrfmt,name,A,dir)
        implicit none
        type(SF),intent(inout) :: A
        type(mesh),intent(inout) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: arrfmt,name
        integer,dimension(2) :: s
        integer :: i,DT
        read(un,*);read(un,*) ! Read tecplot header
        select case (dir)
        case(1); s = (/A%GF(1)%s(2),A%GF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(2); s = (/A%GF(1)%s(1),A%GF(1)%s(3)/); DT = getType_2D(m%g(1),s,name,dir)
        case(3); s = (/A%GF(1)%s(1),A%GF(1)%s(2)/); DT = getType_2D(m%g(1),s,name,dir)
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
        select case (dir)
        case(1); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%GF(i)%f(2,:,:))
                 enddo
        case(2); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%GF(i)%f(:,2,:))
                 enddo
        case(3); do i=1,m%s
                   read(un,*) ! Read tecplot header
                   call imp_2D_1C_g(m%g(i),DT,pad,un,arrfmt,s,dir,A%GF(i)%f(:,:,2))
                 enddo
        case default; stop 'Error: dir must = 1,2,3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      end module