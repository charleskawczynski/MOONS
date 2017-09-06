      module base_export_mod
      use current_precision_mod
      use mesh_extend_mod
      use data_location_mod
      use SF_mod
      use VF_mod
      use GF_export_mod
      use exp_Tecplot_Header_mod
      implicit none

      private
      public :: exp_3D_3C ! exp_3D_3C(m,pad,un,name,U)
      public :: exp_2D_3C ! exp_2D_3C(m,pad,un,name,U,dir,plane)
      public :: exp_1D_3C ! exp_1D_3C(m,pad,un,name,U,dir,line)

      public :: exp_3D_2C ! exp_3D_2C(m,pad,un,name,U,dir)
      public :: exp_2D_2C ! exp_2D_2C(m,pad,un,name,U,dir,plane)
      public :: exp_1D_2C ! exp_1D_2C(m,pad,un,name,U,dir,line)

      public :: exp_3D_1C ! exp_3D_1C(m,pad,un,name,U)
      public :: exp_2D_1C ! exp_2D_1C(m,pad,un,name,U,dir,plane)
      public :: exp_1D_1C ! exp_1D_1C(m,pad,un,name,U,dir,line)

      public :: exp_mesh_SF ! exp_mesh_SF(m,pad,un,name)

      contains

      ! ***********************************************************************
      ! ************************* 3 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine exp_3D_3C(m,pad,un,name,U)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_3D_3C(un,name)
        do i=1,m%s
          call exp_3D_3C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF)
        enddo
      end subroutine

      subroutine exp_2D_3C(m,pad,un,name,U,dir,plane)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_2D_3C(un,dir,name)
        do i=1,m%s
        call exp_2D_3C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
      end subroutine

      subroutine exp_1D_3C(m,pad,un,name,U,dir,line)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: name
        integer,dimension(2),intent(in) :: line
        integer :: i
        call exp_Header_1D_3C(un,dir,name)
        do i=1,m%s
        call exp_1D_3C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
      end subroutine

      ! ***********************************************************************
      ! ************************* 2 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine exp_3D_2C(m,pad,un,name,U,dir)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_3D_2C(un,dir,name)
        select case (dir)
        case(1)
        do i=1,m%s;call exp_3D_2C_GF(m%B(i)%g,U%y%DL,i,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF);enddo
        case(2)
        do i=1,m%s;call exp_3D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF);enddo
        case(3)
        do i=1,m%s;call exp_3D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF);enddo
        case default; stop 'Error: dir must = 1:3 in exp_3D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine exp_2D_2C(m,pad,un,name,U,dir,plane)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_2D_2C(un,dir,name)
        select case (dir)
        case(1)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%y%DL,i,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
        case(2)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
        case(3)
        do i=1,m%s
        call exp_2D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,plane)
        enddo
        case default; stop 'Error: dir must = 1:3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine exp_1D_2C(m,pad,un,name,U,dir,line)
        implicit none
        type(VF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        character(len=*),intent(in) :: name
        integer,dimension(2),intent(in) :: line
        integer :: i
        call exp_Header_1D_2C(un,dir,name)
        select case (dir)
        case(1)
        do i=1,m%s
        call exp_1D_2C_GF(m%B(i)%g,U%y%DL,i,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
        case(2)
        do i=1,m%s
        call exp_1D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
        case(3)
        do i=1,m%s
        call exp_1D_2C_GF(m%B(i)%g,U%x%DL,i,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,line)
        enddo
        case default; stop 'Error: dir must = 1:3 in exp_1D_2C in export_SF.f90'
        end select
      end subroutine

      ! ***********************************************************************
      ! ************************* 1 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine exp_3D_1C(m,pad,un,name,U)
        implicit none
        type(SF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_3D_1C(un,name)
        do i=1,m%s; call exp_3D_1C_GF(m%B(i)%g,U%DL,i,pad,un,U%BF(i)%GF); enddo
      end subroutine

      subroutine exp_2D_1C(m,pad,un,name,U,dir,plane)
        implicit none
        type(SF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_2D_1C(un,dir,name)
        do i=1,m%s; call exp_2D_1C_GF(m%B(i)%g,U%DL,i,pad,un,U%BF(i)%GF,dir,plane); enddo
      end subroutine

      subroutine exp_1D_1C(m,pad,un,name,U,dir,line)
        implicit none
        type(SF),intent(in) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        integer,dimension(2),intent(in) :: line
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_1D_1C(un,dir,name)
        do i=1,m%s; call exp_1D_1C_GF(m%B(i)%g,U%DL,i,pad,un,U%BF(i)%GF,dir,line); enddo
      end subroutine

      subroutine exp_mesh_SF(m,pad,un,name)
        implicit none
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        character(len=*),intent(in) :: name
        integer :: i
        call exp_Header_3D_1C(un,name)
        do i=1,m%s; call exp_3D_0C_GF(m%B(i)%g,DL_Node(),i,pad,un,1.0_cp); enddo
      end subroutine

      end module