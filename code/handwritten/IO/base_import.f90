      module base_import_mod
      use current_precision_mod
      use mesh_extend_mod
      use data_location_mod
      use SF_extend_mod
      use VF_mod
      use GF_import_mod
      use exp_Tecplot_Header_mod
      implicit none

      private
      public :: imp_3D_3C ! imp_3D_3C(m,pad,un,U)
      public :: imp_2D_3C ! imp_2D_3C(m,pad,un,U,dir,plane)
      public :: imp_1D_3C ! imp_1D_3C(m,pad,un,U,dir,line)

      public :: imp_3D_2C ! imp_3D_2C(m,pad,un,U,dir)
      public :: imp_2D_2C ! imp_2D_2C(m,pad,un,U,dir,plane)
      public :: imp_1D_2C ! imp_1D_2C(m,pad,un,U,dir,line)

      public :: imp_3D_1C ! imp_3D_1C(m,pad,un,U)
      public :: imp_2D_1C ! imp_2D_1C(m,pad,un,U,dir,plane)
      public :: imp_1D_1C ! imp_1D_1C(m,pad,un,U,dir,line)

      contains

      ! ***********************************************************************
      ! ************************* 3 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine imp_3D_3C(m,pad,un,U)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s
          call imp_3D_3C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF)
        enddo
      end subroutine

      subroutine imp_2D_3C(m,pad,un,U,dir,plane)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s
        call imp_2D_3C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
      end subroutine

      subroutine imp_1D_3C(m,pad,un,U,dir,line)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        integer,dimension(2),intent(in) :: line
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s
        call imp_1D_3C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
      end subroutine

      ! ***********************************************************************
      ! ************************* 2 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine imp_3D_2C(m,pad,un,U,dir)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        select case (dir)
        case(1)
        do i=1,m%s;call imp_3D_2C_GF(m%B(i)%g,U%y%DL,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF);enddo
        case(2)
        do i=1,m%s;call imp_3D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF);enddo
        case(3)
        do i=1,m%s;call imp_3D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF);enddo
        case default; stop 'Error: dir must = 1:3 in exp_3D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine imp_2D_2C(m,pad,un,U,dir,plane)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        select case (dir)
        case(1)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%y%DL,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
        case(2)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,plane)
        enddo
        case(3)
        do i=1,m%s
        call imp_2D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,plane)
        enddo
        case default; stop 'Error: dir must = 1:3 in exp_2D_2C in export_SF.f90'
        end select
      end subroutine

      subroutine imp_1D_2C(m,pad,un,U,dir,line)
        implicit none
        type(VF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        integer,dimension(2),intent(in) :: line
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        select case (dir)
        case(1)
        do i=1,m%s
        call imp_1D_2C_GF(m%B(i)%g,U%y%DL,pad,un,U%y%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
        case(2)
        do i=1,m%s
        call imp_1D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%z%BF(i)%GF,dir,line)
        enddo
        case(3)
        do i=1,m%s
        call imp_1D_2C_GF(m%B(i)%g,U%x%DL,pad,un,U%x%BF(i)%GF,U%y%BF(i)%GF,dir,line)
        enddo
        case default; stop 'Error: dir must = 1:3 in exp_1D_2C in export_SF.f90'
        end select
      end subroutine

      ! ***********************************************************************
      ! ************************* 1 COMPONENT FIELDS **************************
      ! ***********************************************************************

      subroutine imp_3D_1C(m,pad,un,U)
        implicit none
        type(SF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s; call imp_3D_1C_GF(m%B(i)%g,U%DL,pad,un,U%BF(i)%GF); enddo
      end subroutine

      subroutine imp_2D_1C(m,pad,un,U,dir,plane)
        implicit none
        type(SF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir,plane
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s; call imp_2D_1C_GF(m%B(i)%g,U%DL,pad,un,U%BF(i)%GF,dir,plane); enddo
      end subroutine

      subroutine imp_1D_1C(m,pad,un,U,dir,line)
        implicit none
        type(SF),intent(inout) :: U
        type(mesh),intent(in) :: m
        integer,intent(in) :: pad,un,dir
        integer,dimension(2),intent(in) :: line
        integer :: i
        read(un,*); read(un,*) ! tecplot header
        do i=1,m%s; call imp_1D_1C_GF(m%B(i)%g,U%DL,pad,un,U%BF(i)%GF,dir,line); enddo
      end subroutine

      end module